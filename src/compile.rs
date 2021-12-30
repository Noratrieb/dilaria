//! The compiler that compiles the AST down to bytecode

use crate::ast::{
    Assignment, BinaryOp, BinaryOpKind, Block, Call, Declaration, Expr, FnDecl, Ident, IfStmt,
    Literal, Program, Stmt, UnaryOp, WhileStmt,
};
use crate::bytecode::{FnBlock, Instr, Value};
use crate::errors::{CompilerError, Span};
use crate::value::HashMap;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use std::cell::RefCell;
use std::rc::Rc;

type CResult<T> = Result<T, CompilerError>;

#[derive(Debug, Default)]
struct Env<'ast> {
    locals: HashMap<&'ast str, usize>,
    outer: Option<Rc<RefCell<Env<'ast>>>>,
}

impl Env<'_> {
    fn lookup_local(&self, name: &Ident) -> CResult<usize> {
        fn lookup_inner(env: &Env, name: &Ident) -> Option<usize> {
            env.locals.get(name.sym).copied().or_else(|| {
                env.outer
                    .as_ref()
                    .map(|outer| lookup_inner(&outer.borrow(), name))
                    .flatten()
            })
        }

        lookup_inner(self, name).ok_or_else(|| {
            CompilerError::new(name.span, format!("variable {} not found", name.sym))
        })
    }

    fn new_inner(outer: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            locals: HashMap::default(),
            outer: Some(outer),
        }))
    }
}

#[derive(Debug)]
struct Compiler<'ast, 'bc> {
    blocks: Vec<'bc, FnBlock<'bc>>,
    current_block: usize,
    bump: &'bc Bump,
    /// the current local variables that are in scope, only needed for compiling
    env: Rc<RefCell<Env<'ast>>>,
}

pub fn compile<'bc>(
    ast: &Program,
    bytecode_bump: &'bc Bump,
) -> Result<Vec<'bc, FnBlock<'bc>>, CompilerError> {
    let mut compiler = Compiler {
        blocks: Vec::new_in(bytecode_bump),
        current_block: 0,
        bump: bytecode_bump,
        env: Rc::new(RefCell::new(Default::default())),
    };

    compiler.compile(ast)?;

    Ok(compiler.blocks)
}

impl<'ast, 'bc> Compiler<'ast, 'bc> {
    fn compile(&mut self, ast: &'ast Program<'ast>) -> CResult<()> {
        let global_block = FnBlock {
            code: Vec::new_in(self.bump),
            stack_sizes: Vec::new_in(self.bump),
            spans: Vec::new_in(self.bump),
            arity: 0,
        };
        self.blocks.push(global_block);
        self.current_block = self.blocks.len() - 1;
        self.compile_stmts(&ast.0)?;
        Ok(())
    }

    fn compile_stmts(&mut self, stmts: &'ast [Stmt]) -> CResult<()> {
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

    fn compile_declaration(&mut self, declaration: &'ast Declaration<'ast>) -> CResult<()> {
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

    fn compile_assignment(&mut self, assignment: &Assignment) -> CResult<()> {
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

    fn compile_fn_decl(&mut self, _: &FnDecl) -> CResult<()> {
        todo!()
    }

    fn compile_if(&mut self, _: &IfStmt) -> CResult<()> {
        todo!()
    }

    fn compile_loop(&mut self, _: &Block, _: Span) -> CResult<()> {
        todo!()
    }

    fn compile_while(&mut self, _: &WhileStmt) -> CResult<()> {
        todo!()
    }

    fn compile_break(&mut self, _: Span) -> CResult<()> {
        todo!()
    }

    fn compile_return(&mut self, _: &Option<Expr>, _: Span) -> CResult<()> {
        todo!()
    }

    fn compile_print(&mut self, expr: &Expr, span: Span) -> CResult<()> {
        self.compile_expr(expr)?;

        self.push_instr(Instr::Print, StackChange::Shrink, span);

        Ok(())
    }

    fn compile_block(&mut self, block: &'ast Block) -> CResult<()> {
        let next_env = Env::new_inner(self.env.clone());
        self.env = next_env;

        self.compile_stmts(&block.stmts)?;

        let outer = self.env.borrow().outer.clone().expect("outer env got lost");
        self.env = outer;
        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> CResult<()> {
        match expr {
            Expr::Ident(inner) => self.compile_expr_ident(inner),
            Expr::Literal(inner) => self.compile_expr_literal(inner),
            Expr::UnaryOp(inner) => self.compile_expr_unary(inner),
            Expr::BinaryOp(inner) => self.compile_expr_binary(inner),
            Expr::Call(inner) => self.compile_expr_call(inner),
        }
    }

    fn compile_expr_ident(&mut self, name: &Ident) -> CResult<()> {
        let offset = self.env.borrow().lookup_local(name)?;
        self.push_instr(Instr::Load(offset), StackChange::Grow, name.span);
        Ok(())
    }

    fn compile_expr_literal(&mut self, lit: &Literal) -> CResult<()> {
        let value = match lit {
            Literal::String(_str, _) => Value::String,
            Literal::Number(num, _) => Value::Num(*num),
            Literal::Array(vec, _) => {
                if vec.is_empty() {
                    Value::Array
                } else {
                    todo!()
                }
            }
            Literal::Object(_) => Value::Object(HashMap::default()),
            Literal::Boolean(bool, _) => Value::Bool(*bool),
            Literal::Null(_) => Value::Null,
        };

        self.push_instr(
            Instr::PushVal(self.bump.alloc(value)),
            StackChange::Grow,
            lit.span(),
        );

        Ok(())
    }

    fn compile_expr_unary(&mut self, unary: &UnaryOp) -> CResult<()> {
        self.compile_expr(&unary.expr)?;

        // not and neg compile to the same instruction
        self.push_instr(Instr::Neg, StackChange::None, unary.span);

        Ok(())
    }

    fn compile_expr_binary(&mut self, binary: &BinaryOp) -> CResult<()> {
        // todo: is this the correct ordering?
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

    fn compile_expr_call(&mut self, _: &Call) -> CResult<()> {
        todo!()
    }

    fn current_stack_top(&self) -> usize {
        let block = &self.blocks[self.current_block];
        // we want the stack position, not the size, so the `- 1`
        *block.stack_sizes.last().expect("empty stack") - 1
    }

    fn push_instr(&mut self, instr: Instr<'bc>, stack_change: StackChange, span: Span) {
        let block = &mut self.blocks[self.current_block];
        let stack_top = block.stack_sizes.last().copied().unwrap_or(0);
        let new_stack_top = stack_top as isize + stack_change as isize;
        assert!(new_stack_top >= 0, "instruction popped stack below 0");
        let new_stack_top = new_stack_top as usize;

        block.code.push(instr);
        block.stack_sizes.push(new_stack_top);
        block.spans.push(span);

        debug_assert_eq!(block.code.len(), block.stack_sizes.len());
        debug_assert_eq!(block.code.len(), block.spans.len());
    }
}

#[derive(Debug, Copy, Clone)]
#[repr(i8)]
enum StackChange {
    Shrink = -1,
    None = 0,
    Grow = 1,
}

#[cfg(test)]
mod test {}
