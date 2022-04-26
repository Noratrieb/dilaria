//! The compiler that compiles the AST down to bytecode

use std::{cell::RefCell, rc::Rc};

use bumpalo::{collections::Vec, Bump};

use crate::{
    errors::{CompilerError, Span},
    runtime::{
        bytecode::{FnBlock, Instr},
        gc::{RtAlloc, Symbol},
        vm::Value,
    },
    syntax::ast::{
        Assignment, BinaryOp, BinaryOpKind, Block, Call, CallKind, Declaration, ElsePart, Expr,
        FnDecl, Ident, IfStmt, Literal, Program, Stmt, UnaryOp, WhileStmt,
    },
    HashMap,
};

type CResult<T = ()> = Result<T, CompilerError>;

const CALLCONV_OFFSET_DATA: usize = 3;

#[derive(Debug, PartialEq, Eq)]
enum OuterEnvKind {
    Block,
    Closure,
}

#[derive(Debug)]
struct Env {
    locals: HashMap<Symbol, usize>,
    outer: Option<Rc<RefCell<Env>>>,
    outer_kind: OuterEnvKind,
}

impl Env {
    fn lookup_local(&self, name: &Ident) -> CResult<usize> {
        fn lookup_inner(env: &Env, name: &Ident) -> Option<usize> {
            env.locals.get(&name.sym).copied().or_else(|| {
                // TODO: closure handling lol 👀
                if env.outer_kind == OuterEnvKind::Closure {
                    return None;
                }

                env.outer
                    .as_ref()
                    .and_then(|outer| lookup_inner(&outer.borrow(), name))
            })
        }

        lookup_inner(self, name).ok_or_else(|| {
            CompilerError::new(
                name.span,
                format!("variable {} not found", name.sym.as_str()),
            )
        })
    }

    fn new_inner(outer: Rc<RefCell<Self>>, outer_kind: OuterEnvKind) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            locals: HashMap::default(),
            outer: Some(outer),
            outer_kind,
        }))
    }
}

#[derive(Debug)]
struct Compiler<'bc, 'gc> {
    blocks: Vec<'bc, FnBlock<'bc>>,
    current_block_idx: usize,
    bump: &'bc Bump,
    /// the current local variables that are in scope, only needed for compiling
    env: Rc<RefCell<Env>>,
    rt: &'gc mut RtAlloc,

    /// How nested the current loop is, required for break offsets
    loop_nesting: usize,
    /// All break instructions currently in need of an offset. K=loop_nesting, V=break_indices
    breaks: HashMap<usize, std::vec::Vec<usize>>,
}

pub fn compile<'ast, 'bc, 'gc>(
    ast: &'ast Program,
    bytecode_bump: &'bc Bump,
    rt: &'gc mut RtAlloc,
) -> Result<&'bc [FnBlock<'bc>], CompilerError> {
    let mut compiler = Compiler {
        blocks: Vec::new_in(bytecode_bump),
        current_block_idx: 0,
        bump: bytecode_bump,
        env: Rc::new(RefCell::new(Env {
            locals: HashMap::default(),
            outer: None,
            outer_kind: OuterEnvKind::Block,
        })),
        rt,
        loop_nesting: 0,
        breaks: HashMap::default(),
    };

    compiler.compile(ast)?;

    Ok(compiler.blocks.into_bump_slice())
}

impl<'bc, 'gc> Compiler<'bc, 'gc> {
    fn compile(&mut self, ast: &Program) -> CResult {
        let global_block = FnBlock {
            code: Vec::new_in(self.bump),
            stack_sizes: Vec::new_in(self.bump),
            spans: Vec::new_in(self.bump),
            arity: 0,
        };
        self.blocks.push(global_block);
        self.current_block_idx = self.blocks.len() - 1;

        self.compile_fn_body(ast)?;
        self.push_instr(
            Instr::PushVal(Value::Null),
            StackChange::Grow,
            Span::dummy(),
        );
        // exit the program. here, we use `exit` instead of `return` because there is no stack frame
        self.push_instr(Instr::Exit, StackChange::None, Span::dummy());
        Ok(())
    }

    fn compile_fn_body(&mut self, block: &Block) -> CResult {
        // padding for backwards jumps
        self.push_instr(Instr::Nop, StackChange::None, block.span);

        self.compile_stmts(block.stmts)
    }

    fn compile_stmts(&mut self, stmts: &[Stmt]) -> CResult {
        for stmt in stmts {
            match stmt {
                Stmt::Declaration(inner) => self.compile_declaration(inner),
                Stmt::Assignment(inner) => self.compile_assignment(inner),
                Stmt::FnDecl(inner) => self.compile_fn_decl(inner),
                Stmt::If(inner) => self.compile_if(inner),
                Stmt::Loop(block, span) => self.compile_loop(block, *span),
                Stmt::While(inner) => self.compile_while(inner),
                Stmt::Break(span) => self.compile_break(*span),
                Stmt::Return(expr, span) => self.compile_return(expr, *span),
                Stmt::Print(expr, span) => self.compile_print(expr, *span),
                Stmt::Block(inner) => self.compile_block(inner),
                Stmt::Expr(inner) => self.compile_expr(inner),
            }?;
        }

        Ok(())
    }

    fn compile_declaration(&mut self, declaration: &Declaration) -> CResult {
        // Compile the expression, the result of the expression will be the last thing left on the stack
        self.compile_expr(&declaration.init)?;
        // Now just remember that the value at this stack location is this variable name
        let stack_pos = self.current_stack_top();
        self.env
            .borrow_mut()
            .locals
            .insert(declaration.name.sym, stack_pos);
        Ok(())
    }

    fn compile_assignment(&mut self, assignment: &Assignment) -> CResult {
        let local = match &assignment.lhs {
            Expr::Ident(ident) => ident,
            _ => todo!(),
        };

        let stack_pos = self.env.borrow().lookup_local(local)?;

        self.compile_expr(&assignment.rhs)?;

        self.push_instr(
            Instr::Store(stack_pos),
            StackChange::Shrink,
            assignment.span,
        );

        Ok(())
    }

    fn compile_fn_decl(&mut self, decl: &FnDecl) -> CResult {
        let block = FnBlock {
            code: Vec::new_in(self.bump),
            stack_sizes: Vec::new_in(self.bump),
            spans: Vec::new_in(self.bump),
            arity: decl.params.len().try_into().map_err(|_| {
                CompilerError::new(
                    decl.params[u8::MAX as usize]
                        .span
                        .extend(decl.params.last().unwrap().span),
                    "Too many parameters. How the fuck did you do this.".to_string(),
                )
            })?,
        };

        // set the new block as the current block
        let new_block_idx = self.blocks.len();
        self.blocks.push(block);
        let old_block_idx = self.current_block_idx;
        self.current_block_idx = new_block_idx;

        // compile the body with a captured environment
        let inner_env = Env::new_inner(self.env.clone(), OuterEnvKind::Closure);
        self.env = inner_env;

        {
            // insert params as locals
            let mut env_mut = self.env.borrow_mut();
            for (i, param) in decl.params.iter().enumerate() {
                env_mut.locals.insert(param.sym, i);
            }

            let block = &mut self.blocks[self.current_block_idx];
            block.code.push(Instr::Nop);
            block.spans.push(decl.span);
            block
                .stack_sizes
                .push(decl.params.len() + CALLCONV_OFFSET_DATA);
        }

        self.compile_stmts(decl.body.stmts)?;

        self.push_instr(Instr::PushVal(Value::Null), StackChange::Grow, decl.span);
        self.push_instr(Instr::Return, StackChange::None, decl.span);

        let outer = self.env.borrow().outer.clone().expect("outer env got lost");
        self.env = outer;

        self.current_block_idx = old_block_idx;

        // save the function as a local variable
        self.push_instr(
            Instr::PushVal(Value::Function(new_block_idx)),
            StackChange::Grow,
            decl.span,
        );

        let stack_pos = self.current_stack_top();

        self.env
            .borrow_mut()
            .locals
            .insert(decl.name.sym, stack_pos);

        Ok(())
    }

    fn compile_if(&mut self, if_stmt: &IfStmt) -> CResult {
        /*
           0 PushVal (true)
         ╭─1 JumpCond (2)
         │ 2 // it is true
        ╭│─4 Jmp (1)           │ this is optional only for else
        │╰>5 // it it false    │
        ╰─>7 // continue here
          */

        self.compile_expr(&if_stmt.cond)?;

        // the offset will be fixed later
        let jmp_idx = self.push_instr(Instr::JmpFalse(0), StackChange::Shrink, if_stmt.span);

        self.compile_block(&if_stmt.body)?;

        if let Some(else_part) = if_stmt.else_part {
            let else_skip_jmp_idx = self.push_instr(Instr::Jmp(0), StackChange::None, if_stmt.span);

            let jmp_pos = self.forward_jmp_offset(jmp_idx as isize);

            self.change_instr(jmp_idx, Instr::JmpFalse(jmp_pos));

            match else_part {
                ElsePart::Else(block, _) => {
                    self.compile_block(block)?;
                }
                ElsePart::ElseIf(if_stmt, _) => {
                    self.compile_if(if_stmt)?;
                }
            }

            let jmp_pos = self.forward_jmp_offset(else_skip_jmp_idx as isize);

            self.change_instr(else_skip_jmp_idx, Instr::Jmp(jmp_pos));
        } else {
            let jmp_pos = self.forward_jmp_offset(jmp_idx as isize);
            self.change_instr(jmp_idx, Instr::JmpFalse(jmp_pos));
        }

        Ok(())
    }

    fn compile_loop(&mut self, ast_block: &Block, span: Span) -> CResult {
        /*
        ╭>0 // do things
        ╰─1 JMP (-2),
          */

        let first_stmt_idx = self.code_len();
        let pre_loop_stack_size = self.current_stack_size();

        self.loop_nesting += 1;

        self.compile_block(ast_block)?;

        self.shrink_stack(pre_loop_stack_size, span);
        let jmp_offset = self.back_jmp_offset(first_stmt_idx);
        self.push_instr(Instr::Jmp(jmp_offset), StackChange::None, span);

        self.end_loop();

        Ok(())
    }

    fn compile_while(&mut self, while_stmt: &WhileStmt) -> CResult {
        /*
        ╭─>0 PushVal (true)
        │╭─1 JmpFalse (2)
        ││ 2 // body
        ╰│─3 Jmp (-3)
         ╰>4 // continue here
          */

        let cond_index = self.code_len();
        let pre_loop_stack_size = self.current_stack_size();
        self.loop_nesting += 1;

        self.compile_expr(&while_stmt.cond)?;

        let jmp_false_idx =
            self.push_instr(Instr::JmpFalse(0), StackChange::Shrink, while_stmt.span);

        self.compile_block(&while_stmt.body)?;

        self.shrink_stack(pre_loop_stack_size, while_stmt.span);
        let jmp_offset = self.back_jmp_offset(cond_index);
        self.push_instr(Instr::Jmp(jmp_offset), StackChange::None, while_stmt.span);

        let jmp_offset = self.forward_jmp_offset(jmp_false_idx as isize);
        self.change_instr(jmp_false_idx, Instr::JmpFalse(jmp_offset));

        self.end_loop();

        Ok(())
    }

    fn compile_break(&mut self, span: Span) -> CResult {
        let break_idx = self.push_instr(Instr::Jmp(0), StackChange::None, span);
        self.breaks
            .entry(self.loop_nesting)
            .or_default()
            .push(break_idx);
        Ok(())
    }

    fn compile_return(&mut self, expr: &Option<Expr>, span: Span) -> CResult {
        if let Some(expr) = expr {
            self.compile_expr(expr)?;
        } else {
            self.push_instr(Instr::PushVal(Value::Null), StackChange::Grow, span);
        }

        self.push_instr(Instr::Return, StackChange::None, span);

        Ok(())
    }

    fn compile_print(&mut self, expr: &Expr, span: Span) -> CResult {
        self.compile_expr(expr)?;

        self.push_instr(Instr::Print, StackChange::Shrink, span);

        Ok(())
    }

    fn compile_block(&mut self, block: &Block) -> CResult {
        let next_env = Env::new_inner(self.env.clone(), OuterEnvKind::Block);
        self.env = next_env;

        self.compile_stmts(block.stmts)?;

        let outer = self.env.borrow().outer.clone().expect("outer env got lost");
        self.env = outer;
        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> CResult {
        match expr {
            Expr::Ident(inner) => self.compile_expr_ident(inner),
            Expr::Literal(inner) => self.compile_expr_literal(inner),
            Expr::UnaryOp(inner) => self.compile_expr_unary(inner),
            Expr::BinaryOp(inner) => self.compile_expr_binary(inner),
            Expr::Call(inner) => self.compile_expr_call(inner),
        }
    }

    fn compile_expr_ident(&mut self, name: &Ident) -> CResult {
        let offset = self.env.borrow().lookup_local(name)?;
        self.push_instr(Instr::Load(offset), StackChange::Grow, name.span);
        Ok(())
    }

    fn compile_expr_literal(&mut self, lit: &Literal) -> CResult {
        let value = match lit {
            Literal::String(str, _) => Value::String(*str),
            Literal::Number(num, _) => Value::Num(*num),
            Literal::Array(vec, _) => {
                if vec.is_empty() {
                    Value::Array
                } else {
                    todo!()
                }
            }
            Literal::Object(_) => Value::Object(self.rt.alloc_obj(HashMap::default())),
            Literal::Boolean(bool, _) => Value::Bool(*bool),
            Literal::Null(_) => Value::Null,
        };

        self.push_instr(Instr::PushVal(value), StackChange::Grow, lit.span());

        Ok(())
    }

    fn compile_expr_unary(&mut self, unary: &UnaryOp) -> CResult {
        self.compile_expr(&unary.expr)?;

        // not and neg compile to the same instruction
        self.push_instr(Instr::Neg, StackChange::None, unary.span);

        Ok(())
    }

    fn compile_expr_binary(&mut self, binary: &BinaryOp) -> CResult {
        self.compile_expr(&binary.lhs)?;
        self.compile_expr(&binary.rhs)?;

        let instruction = match binary.kind {
            BinaryOpKind::Add => Instr::BinAdd,
            BinaryOpKind::And => Instr::BinAnd,
            BinaryOpKind::Or => Instr::BinOr,
            BinaryOpKind::Equal => Instr::CmpEq,
            BinaryOpKind::GreaterEqual => Instr::CmpGreaterEq,
            BinaryOpKind::Greater => Instr::CmpGreater,
            BinaryOpKind::LessEqual => Instr::CmpLessEq,
            BinaryOpKind::Less => Instr::CmpLess,
            BinaryOpKind::NotEqual => Instr::CmpNotEq,
            BinaryOpKind::Sub => Instr::BinSub,
            BinaryOpKind::Mul => Instr::BinMul,
            BinaryOpKind::Div => Instr::BinDiv,
            BinaryOpKind::Mod => Instr::BinMod,
        };

        self.push_instr(instruction, StackChange::Shrink, binary.span);

        Ok(())
    }

    fn compile_expr_call(&mut self, call: &Call) -> CResult {
        let params = match &call.kind {
            CallKind::Fn(params) => params,
            _ => todo!(),
        };

        let name = match &call.callee {
            Expr::Ident(ident) => ident,
            _ => todo!(),
        };

        let offset = self.env.borrow().lookup_local(name)?;

        for param in params.iter() {
            self.compile_expr(param)?;
        }

        self.push_instr(Instr::Load(offset), StackChange::Grow, call.span);
        self.push_instr(Instr::Call, StackChange::None, call.span);

        Ok(())
    }

    fn shrink_stack(&mut self, jmp_target_stack_size: usize, span: Span) {
        let amount = self.current_stack_size() - jmp_target_stack_size;

        if amount == 0 {
            return;
        }

        self.push_instr(
            Instr::ShrinkStack(amount),
            StackChange::ShrinkN(amount),
            span,
        );
    }

    fn end_loop(&mut self) {
        let breaks = self.breaks.remove(&self.loop_nesting);
        if let Some(breaks) = breaks {
            for brk in breaks {
                let offset = self.forward_jmp_offset(brk as isize);
                self.change_instr(brk, Instr::Jmp(offset));
            }
        }
        self.loop_nesting -= 1;
    }

    fn current_stack_top(&self) -> usize {
        let block = &self.blocks[self.current_block_idx];
        // we want the stack position, not the size, so the `- 1`
        *block.stack_sizes.last().expect("empty stack") - 1
    }

    /// source is implicitly: self.code_len()
    fn back_jmp_offset(&self, target: isize) -> isize {
        let source = self.code_len();
        -(source - target + 1)
    }

    /// target is implicitly: self.code_len()
    fn forward_jmp_offset(&self, source: isize) -> isize {
        let target = self.code_len();
        target - (source) - 1
    }

    fn code_len(&self) -> isize {
        let block = &self.blocks[self.current_block_idx];
        block.code.len() as isize
    }

    fn current_stack_size(&self) -> usize {
        let block = &self.blocks[self.current_block_idx];
        block.stack_sizes.last().copied().unwrap_or(0)
    }

    fn change_instr(&mut self, index: usize, instr: Instr) {
        let block = &mut self.blocks[self.current_block_idx];
        block.code[index] = instr;
    }

    /// Pushes an instruction and returns the index of the new instruction
    fn push_instr(&mut self, instr: Instr, stack_change: StackChange, span: Span) -> usize {
        let block = &mut self.blocks[self.current_block_idx];
        let stack_top = block.stack_sizes.last().copied().unwrap_or(0);
        let new_stack_top = stack_top as isize + stack_change.as_isize();
        assert!(new_stack_top >= 0, "instruction popped stack below 0");
        let new_stack_top = new_stack_top as usize;

        block.code.push(instr);
        block.stack_sizes.push(new_stack_top);
        block.spans.push(span);

        debug_assert_eq!(block.code.len(), block.stack_sizes.len());
        debug_assert_eq!(block.code.len(), block.spans.len());

        block.code.len() - 1
    }
}

#[derive(Debug, Copy, Clone)]
enum StackChange {
    Shrink,
    None,
    Grow,
    ShrinkN(usize),
}

impl StackChange {
    fn as_isize(&self) -> isize {
        match self {
            StackChange::Shrink => -1,
            StackChange::None => 0,
            StackChange::Grow => 1,
            StackChange::ShrinkN(n) => -(*n as isize),
        }
    }
}
