use crate::ast::{
    Assignment, BinaryOp, BinaryOpKind, Block, Call, Declaration, Expr, FnDecl, Ident, IfStmt,
    Literal, Program, Stmt, UnaryOp, WhileStmt,
};
use crate::bytecode::{FnBlock, Instr, Value};
use crate::errors::{CompilerError, Span};
use crate::value::HashMap;

type CResult<T> = Result<T, CompileError>;

#[derive(Debug, Default)]
struct Compiler {
    blocks: Vec<FnBlock>,
    current_block: usize,
    /// the current local variables that are in scope, only needed for compiling
    locals: Vec<HashMap<Ident, usize>>,
}

pub fn compile(ast: &Program) -> Result<Vec<FnBlock>, CompileError> {
    let mut compiler = Compiler::default();

    compiler.compile(ast)?;

    Ok(compiler.blocks)
}

impl Compiler {
    fn compile(&mut self, ast: &Program) -> CResult<()> {
        let global_block = FnBlock::default();
        self.blocks.push(global_block);
        self.current_block = self.blocks.len() - 1;
        self.locals.push(HashMap::default());
        self.compile_fn_block(&ast.0)?;
        Ok(())
    }

    fn compile_fn_block(&mut self, stmts: &[Stmt]) -> CResult<()> {
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
                Stmt::Block(inner) => self.compile_block(inner),
                Stmt::Expr(inner) => self.compile_expr(inner),
            }?;
        }

        Ok(())
    }

    fn compile_declaration(&mut self, declaration: &Declaration) -> CResult<()> {
        // Compile the expression, the result of the expression will be the last thing left on the stack
        self.compile_expr(&declaration.init)?;
        // Now just remember that the value at this stack location is this variable name
        let stack_pos = self.current_stack_top();
        self.locals().insert(declaration.name.clone(), stack_pos);
        Ok(())
    }

    fn compile_assignment(&mut self, assignment: &Assignment) -> CResult<()> {
        let local = match &assignment.lhs {
            Expr::Ident(ident) => ident,
            _ => todo!(),
        };

        let stack_pos = self.lookup_local(local)?;

        self.compile_expr(&assignment.rhs)?;

        self.push_instr(Instr::Store(stack_pos), StackChange::Shrink);

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

    fn compile_block(&mut self, _: &Block) -> CResult<()> {
        todo!()
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
        let offset = self.lookup_local(name)?;
        self.push_instr(Instr::Load(offset), StackChange::Grow);
        Ok(())
    }

    fn compile_expr_literal(&mut self, lit: &Literal) -> CResult<()> {
        let value = match lit {
            Literal::String(str, _) => Value::String(str.clone().into()),
            Literal::Number(num, _) => Value::Num(*num),
            Literal::Array(vec, _) => {
                if vec.is_empty() {
                    Value::Array(Vec::new())
                } else {
                    todo!()
                }
            }
            Literal::Object(_) => Value::Object(HashMap::default()),
            Literal::Boolean(bool, _) => Value::Bool(*bool),
            Literal::Null(_) => Value::Null,
        };

        self.push_instr(Instr::PushVal(Box::new(value)), StackChange::Grow);

        Ok(())
    }

    fn compile_expr_unary(&mut self, inner: &UnaryOp) -> CResult<()> {
        self.compile_expr(&inner.expr)?;

        // not and neg compile to the same instruction
        self.push_instr(Instr::Neg, StackChange::None);

        Ok(())
    }

    fn compile_expr_binary(&mut self, inner: &BinaryOp) -> CResult<()> {
        // todo: is this the correct ordering?
        self.compile_expr(&inner.lhs)?;
        self.compile_expr(&inner.rhs)?;

        let instruction = match inner.kind {
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

        self.push_instr(instruction, StackChange::Shrink);

        Ok(())
    }

    fn compile_expr_call(&mut self, _: &Call) -> CResult<()> {
        todo!()
    }

    fn locals(&mut self) -> &mut HashMap<Ident, usize> {
        self.locals.last_mut().expect("no locals found")
    }

    fn lookup_local(&self, name: &Ident) -> CResult<usize> {
        for locals in self.locals.iter().rev() {
            if let Some(&position) = locals.get(name) {
                return Ok(position);
            }
        }

        Err(CompileError)
    }

    fn current_stack_top(&self) -> usize {
        let block = &self.blocks[self.current_block];
        *block.stack_sizes.last().expect("empty stack")
    }

    fn push_instr(&mut self, instr: Instr, stack_change: StackChange) {
        let block = &mut self.blocks[self.current_block];
        let stack_top = block.stack_sizes.last().copied().unwrap_or(0);
        let new_stack_top = stack_top as isize + stack_change as isize;
        assert!(new_stack_top >= 0, "instruction popped stack below 0");
        let new_stack_top = new_stack_top as usize;

        block.code.push(instr);
        block.stack_sizes.push(new_stack_top);
    }
}

#[derive(Debug, Copy, Clone)]
#[repr(i8)]
enum StackChange {
    Shrink = -1,
    None = 0,
    Grow = 1,
}

#[derive(Debug)]
pub struct CompileError;

impl CompilerError for CompileError {
    fn span(&self) -> Span {
        todo!()
    }

    fn message(&self) -> String {
        todo!()
    }

    fn note(&self) -> Option<String> {
        todo!()
    }
}
