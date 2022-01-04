//! The parser implementation.
//!
//! It's a handwritten recursive descent parser. It has an internal peekable iterator from where
//! it gets its next tokens. Only a lookahead of one is required.

#[cfg(test)]
mod test;

use crate::ast::*;
use crate::errors::{CompilerError, Span};
use crate::lex::{Token, TokenKind};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use std::iter::Peekable;

#[derive(Debug)]
struct Parser<'ast, I>
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
    depth: usize,
    inside_fn_depth: usize,
    inside_loop_depth: usize,
    bump: &'ast Bump,
}

pub fn parse<'lexer, 'ast>(
    tokens: impl Iterator<Item = Token> + 'lexer,
    ast_bump: &'ast Bump,
) -> Result<Program<'ast>, CompilerError> {
    let mut parser = Parser {
        tokens: tokens.peekable(),
        depth: 0,
        inside_fn_depth: 0,
        inside_loop_depth: 0,
        bump: ast_bump,
    };
    let program = parser.program()?;
    Ok(program)
}

type ParseResult<T> = Result<T, CompilerError>;

macro_rules! parse_bin_op {
    ($self: ident, $lhs: ident, $kind: expr, $function: ident) => {{
        let _ = $self.next();
        let rhs = $self.$function()?;
        Ok(Expr::BinaryOp($self.bump.alloc(BinaryOp {
            span: $lhs.span().extend(rhs.span()),
            lhs: $lhs,
            rhs,
            kind: $kind,
        })))
    }};
}

macro_rules! exit_parse {
    ($self: ident) => {
        $self.depth -= 1;
    };
}

macro_rules! enter_parse {
    ($self: ident) => {
        $self.depth += 1;

        if $self.depth > Self::MAX_DEPTH {
            let _ = $self.too_nested_error()?;
        }
    };
}

impl<'ast, I> Parser<'ast, I>
where
    I: Iterator<Item = Token>,
{
    const MAX_DEPTH: usize = 100;

    fn program(&mut self) -> ParseResult<Program<'ast>> {
        Ok(Program(self.statement_list()?))
    }

    fn too_nested_error(&mut self) -> ParseResult<()> {
        let next_token = self.next();
        match next_token {
            Some(token) => Err(CompilerError::new(
                token.span,
                "reached maximal nesting depth".to_string(),
            )),
            None => Err(CompilerError::eof(
                "reached EOF while being nested to deeply",
            )),
        }
    }

    fn statement_list(&mut self) -> ParseResult<Vec<'ast, Stmt<'ast>>> {
        enter_parse!(self);
        let mut stmts = Vec::new_in(self.bump);
        let return_stmts = loop {
            if let Some(TokenKind::BraceC) | None = self.peek_kind() {
                break Ok(stmts);
            }
            let stmt = self.statement()?;
            stmts.push(stmt);
        };
        exit_parse!(self);
        return_stmts
    }

    fn block(&mut self) -> ParseResult<Block<'ast>> {
        enter_parse!(self);

        let start_span = self.expect(TokenKind::BraceO)?.span;
        let stmts = self.statement_list()?;
        let end_span = self.expect(TokenKind::BraceC)?.span;

        exit_parse!(self);

        Ok(Block {
            stmts,
            span: start_span.extend(end_span),
        })
    }

    fn statement(&mut self) -> ParseResult<Stmt<'ast>> {
        enter_parse!(self);

        let stmt = match *self
            .peek_kind()
            .ok_or_else(|| CompilerError::eof("statement"))?
        {
            TokenKind::Let => self.declaration(),
            TokenKind::Fn => self.fn_decl(),
            TokenKind::If => Ok(Stmt::If(self.if_stmt()?)),
            TokenKind::Loop => self.loop_stmt(),
            TokenKind::While => self.while_stmt(),
            TokenKind::Break => self.break_stmt(),
            TokenKind::Return => self.return_stmt(),
            TokenKind::Print => self.print_stmt(),
            TokenKind::BraceO => Ok(Stmt::Block(self.block()?)),
            _ => {
                let stmt = self.assignment()?;
                Ok(stmt)
            }
        };
        exit_parse!(self);
        stmt
    }

    fn declaration(&mut self) -> ParseResult<Stmt<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenKind::Let)?.span;
        let name = self.ident()?;
        self.expect(TokenKind::Equal)?;
        let init = self.expression()?;
        self.expect(TokenKind::Semi)?;

        exit_parse!(self);

        Ok(Stmt::Declaration(Declaration {
            span: keyword_span.extend(init.span()),
            name,
            init,
        }))
    }

    fn fn_decl(&mut self) -> ParseResult<Stmt<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenKind::Fn)?.span;
        let name = self.ident()?;
        let args = self.fn_args()?;

        self.inside_fn_depth += 1;
        let body = self.block()?;
        self.inside_fn_depth -= 1;

        exit_parse!(self);

        Ok(Stmt::FnDecl(FnDecl {
            span: keyword_span.extend(body.span),
            name,
            params: args,
            body,
        }))
    }

    fn fn_args(&mut self) -> ParseResult<Vec<'ast, Ident>> {
        enter_parse!(self);

        self.expect(TokenKind::ParenO)?;
        let params = self.parse_list(TokenKind::ParenC, Self::ident)?;
        self.expect(TokenKind::ParenC)?;

        exit_parse!(self);

        Ok(params)
    }

    fn if_stmt(&mut self) -> ParseResult<IfStmt<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenKind::If)?.span;
        let cond = self.expression()?;
        let body = self.block()?;

        let else_part = if let Some(TokenKind::Else) = self.peek_kind() {
            Some(self.else_part()?)
        } else {
            None
        };

        exit_parse!(self);

        Ok(IfStmt {
            span: keyword_span
                .extend(body.span)
                .option_extend(else_part.as_ref().map(ElsePart::span)),
            cond,
            body,
            else_part: else_part.map(|part| &*self.bump.alloc(part)),
        })
    }

    fn else_part(&mut self) -> ParseResult<ElsePart<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenKind::Else)?.span;

        let else_part = if let Some(TokenKind::If) = self.peek_kind() {
            let else_if_stmt = self.if_stmt()?;
            let else_span = keyword_span.extend(else_if_stmt.span);
            Ok(ElsePart::ElseIf(else_if_stmt, else_span))
        } else {
            let block = self.block()?;
            let else_span = keyword_span.extend(block.span);
            Ok(ElsePart::Else(block, else_span))
        };

        exit_parse!(self);

        else_part
    }

    fn loop_stmt(&mut self) -> ParseResult<Stmt<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenKind::Loop)?.span;

        self.inside_loop_depth += 1;
        let block = self.block()?;
        self.inside_loop_depth -= 1;

        let loop_span = keyword_span.extend(block.span);

        exit_parse!(self);

        Ok(Stmt::Loop(block, keyword_span.extend(loop_span)))
    }

    fn while_stmt(&mut self) -> ParseResult<Stmt<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenKind::While)?.span;
        let cond = self.expression()?;

        self.inside_loop_depth += 1;
        let body = self.block()?;
        self.inside_loop_depth -= 1;

        exit_parse!(self);

        Ok(Stmt::While(WhileStmt {
            span: keyword_span.extend(body.span),
            cond,
            body,
        }))
    }

    fn break_stmt(&mut self) -> ParseResult<Stmt<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenKind::Break)?.span;
        let semi_span = self.expect(TokenKind::Semi)?.span;

        exit_parse!(self);

        if self.inside_loop_depth == 0 {
            Err(CompilerError::new(
                keyword_span.extend(semi_span),
                "break used outside of loop".to_string(),
            ))
        } else {
            Ok(Stmt::Break(keyword_span.extend(semi_span)))
        }
    }

    fn return_stmt(&mut self) -> ParseResult<Stmt<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenKind::Return)?.span;

        let expr = if let Some(TokenKind::Semi) = self.peek_kind() {
            None
        } else {
            Some(self.expression()?)
        };

        let semi_span = self.expect(TokenKind::Semi)?.span;

        exit_parse!(self);

        if self.inside_fn_depth == 0 {
            Err(CompilerError::new(
                keyword_span.extend(semi_span),
                "return used outside of function".to_string(),
            ))
        } else {
            Ok(Stmt::Return(expr, keyword_span.extend(semi_span)))
        }
    }

    fn print_stmt(&mut self) -> ParseResult<Stmt<'ast>> {
        enter_parse!(self);

        let print_span = self.expect(TokenKind::Print)?.span;

        let expr = self.expression()?;

        let semi_span = self.expect(TokenKind::Semi)?.span;

        exit_parse!(self);

        Ok(Stmt::Print(expr, print_span.extend(semi_span)))
    }

    fn assignment(&mut self) -> ParseResult<Stmt<'ast>> {
        enter_parse!(self);

        let expr = self.expression()?;

        let stmt = if let Some(TokenKind::Equal) = self.peek_kind() {
            let _ = self.expect(TokenKind::Equal)?;
            let init = self.expression()?;
            let semi_span = self.expect(TokenKind::Semi)?.span;
            Ok(Stmt::Assignment(Assignment {
                span: expr.span().extend(semi_span),
                lhs: expr,
                rhs: init,
            }))
        } else {
            let _ = self.expect(TokenKind::Semi)?;
            Ok(Stmt::Expr(expr))
        };

        exit_parse!(self);
        stmt
    }

    fn expression(&mut self) -> ParseResult<Expr<'ast>> {
        enter_parse!(self);
        let return_expr = self.logical_or();
        exit_parse!(self);
        return_expr
    }

    fn logical_or(&mut self) -> ParseResult<Expr<'ast>> {
        enter_parse!(self);

        let lhs = self.logical_and()?;
        let return_expr = match self.peek_kind() {
            Some(TokenKind::Or) => parse_bin_op!(self, lhs, BinaryOpKind::Or, logical_or),
            _ => Ok(lhs),
        };

        exit_parse!(self);
        return_expr
    }

    fn logical_and(&mut self) -> ParseResult<Expr<'ast>> {
        enter_parse!(self);

        let lhs = self.equality()?;
        let return_expr = match self.peek_kind() {
            Some(TokenKind::And) => parse_bin_op!(self, lhs, BinaryOpKind::And, logical_and),
            _ => Ok(lhs),
        };

        exit_parse!(self);
        return_expr
    }

    fn equality(&mut self) -> ParseResult<Expr<'ast>> {
        enter_parse!(self);

        let lhs = self.comparison()?;
        let return_expr = match self.peek_kind() {
            Some(TokenKind::BangEqual) => {
                parse_bin_op!(self, lhs, BinaryOpKind::NotEqual, comparison)
            }
            Some(TokenKind::EqualEqual) => {
                parse_bin_op!(self, lhs, BinaryOpKind::Equal, comparison)
            }
            _ => Ok(lhs),
        };
        exit_parse!(self);
        return_expr
    }

    fn comparison(&mut self) -> ParseResult<Expr<'ast>> {
        enter_parse!(self);

        let lhs = self.term()?;
        let return_expr = match self.peek_kind() {
            Some(TokenKind::Greater) => parse_bin_op!(self, lhs, BinaryOpKind::Greater, term),
            Some(TokenKind::GreaterEqual) => {
                parse_bin_op!(self, lhs, BinaryOpKind::GreaterEqual, term)
            }
            Some(TokenKind::Less) => parse_bin_op!(self, lhs, BinaryOpKind::Less, term),
            Some(TokenKind::LessEqual) => {
                parse_bin_op!(self, lhs, BinaryOpKind::LessEqual, term)
            }
            _ => Ok(lhs),
        };
        exit_parse!(self);
        return_expr
    }

    fn term(&mut self) -> ParseResult<Expr<'ast>> {
        enter_parse!(self);

        let lhs = self.factor()?;
        let return_expr = match self.peek_kind() {
            Some(TokenKind::Plus) => parse_bin_op!(self, lhs, BinaryOpKind::Add, term),
            Some(TokenKind::Minus) => parse_bin_op!(self, lhs, BinaryOpKind::Sub, term),
            _ => Ok(lhs),
        };
        exit_parse!(self);
        return_expr
    }

    fn factor(&mut self) -> ParseResult<Expr<'ast>> {
        enter_parse!(self);

        let lhs = self.unary()?;
        let return_expr = match self.peek_kind() {
            Some(TokenKind::Asterisk) => parse_bin_op!(self, lhs, BinaryOpKind::Mul, factor),
            Some(TokenKind::Slash) => parse_bin_op!(self, lhs, BinaryOpKind::Div, factor),
            Some(TokenKind::Percent) => parse_bin_op!(self, lhs, BinaryOpKind::Mod, factor),
            _ => Ok(lhs),
        };
        exit_parse!(self);
        return_expr
    }

    fn unary(&mut self) -> ParseResult<Expr<'ast>> {
        enter_parse!(self);

        let return_expr = match self.peek_kind() {
            Some(TokenKind::Not) => {
                let unary_op_span = self.next().unwrap().span;
                let expr = self.call()?;
                Ok(Expr::UnaryOp(self.bump.alloc(UnaryOp {
                    span: unary_op_span.extend(expr.span()),
                    expr,
                    kind: UnaryOpKind::Not,
                })))
            }
            Some(TokenKind::Minus) => {
                let unary_op_span = self.next().unwrap().span;
                let expr = self.call()?;
                Ok(Expr::UnaryOp(self.bump.alloc(UnaryOp {
                    span: unary_op_span.extend(expr.span()),
                    expr,
                    kind: UnaryOpKind::Neg,
                })))
            }
            _ => self.call(),
        };
        exit_parse!(self);
        return_expr
    }

    fn call(&mut self) -> ParseResult<Expr<'ast>> {
        enter_parse!(self);

        let mut expr = self.primary()?;

        loop {
            expr = match self.peek_kind() {
                Some(TokenKind::ParenO) => {
                    let open_span = self.expect(TokenKind::ParenO)?.span;
                    let args = self.parse_list(TokenKind::ParenC, Self::expression)?;
                    let close_span = self.expect(TokenKind::ParenC)?.span;

                    Expr::Call(self.bump.alloc(Call {
                        callee: expr,
                        span: open_span.extend(close_span),
                        kind: CallKind::Fn(args),
                    }))
                }
                Some(TokenKind::Dot) => {
                    let dot_span = self.expect(TokenKind::Dot)?.span;
                    let field = self.ident()?;

                    Expr::Call(self.bump.alloc(Call {
                        callee: expr,
                        span: dot_span.extend(field.span),
                        kind: CallKind::Field(field),
                    }))
                }
                _ => break,
            }
        }

        exit_parse!(self);

        Ok(expr)
    }

    fn primary(&mut self) -> ParseResult<Expr<'ast>> {
        enter_parse!(self);

        let next = self.next().ok_or_else(|| CompilerError::eof("primary"))?;
        let return_expr = match next.kind {
            TokenKind::String(literal) => Ok(Expr::Literal(Literal::String(literal, next.span))),
            TokenKind::Number(literal) => Ok(Expr::Literal(Literal::Number(literal, next.span))),
            TokenKind::False => Ok(Expr::Literal(Literal::Boolean(false, next.span))),
            TokenKind::True => Ok(Expr::Literal(Literal::Boolean(true, next.span))),
            TokenKind::Null => Ok(Expr::Literal(Literal::Null(next.span))),
            TokenKind::BraceO => self.object_literal(next.span),
            TokenKind::BracketO => self.array_literal(next.span),
            TokenKind::ParenO => {
                let expr = self.expression()?;
                let _ = self.expect(TokenKind::ParenC)?;
                Ok(expr)
            }
            TokenKind::Ident(sym) => Ok(Expr::Ident(Ident {
                sym,
                span: next.span,
            })),
            TokenKind::Error(error) => Err(*error),
            _ => Err(CompilerError::new(
                next.span,
                format!("invalid token in expression: `{:?}`", next.kind),
            )),
        };
        exit_parse!(self);
        return_expr
    }

    fn ident(&mut self) -> ParseResult<Ident> {
        enter_parse!(self);

        let Token { kind, span } = self
            .next()
            .ok_or_else(|| CompilerError::eof("identifier"))?;
        let return_expr = match kind {
            TokenKind::Ident(sym) => Ok(Ident { sym, span }),
            TokenKind::Error(error) => Err(*error),
            _ => {
                return Err(CompilerError::new(
                    span,
                    format!("expected identifier, received `{:?}`", kind),
                ))
            }
        };
        exit_parse!(self);
        return_expr
    }

    fn object_literal(&mut self, open_span: Span) -> ParseResult<Expr<'ast>> {
        enter_parse!(self);

        let close_span = self.expect(TokenKind::BraceC)?.span;

        exit_parse!(self);
        Ok(Expr::Literal(Literal::Object(open_span.extend(close_span))))
    }

    fn array_literal(&mut self, open_span: Span) -> ParseResult<Expr<'ast>> {
        enter_parse!(self);

        let elements = self.parse_list(TokenKind::BracketC, Self::expression)?;
        let closing_bracket = self.expect(TokenKind::BracketC)?;

        let return_expr = Ok(Expr::Literal(Literal::Array(
            elements,
            open_span.extend(closing_bracket.span),
        )));
        exit_parse!(self);
        return_expr
    }

    fn parse_list<T, F>(&mut self, close: TokenKind, mut parser: F) -> ParseResult<Vec<'ast, T>>
    where
        F: FnMut(&mut Self) -> ParseResult<T>,
    {
        enter_parse!(self);

        let mut elements = Vec::new_in(self.bump);

        if self.peek_kind() == Some(&close) {
            return Ok(elements);
        }

        let expr = parser(self)?;
        elements.push(expr);

        let reached_eof = || {
            CompilerError::new(
                Span::dummy(),
                format!("reached EOF expecting `{:?}`", close.clone()),
            )
        };

        while self.peek_kind().ok_or_else(reached_eof)? != &close {
            self.expect(TokenKind::Comma)?;

            // trailing comma support
            if self.peek_kind() == Some(&close) {
                break;
            }

            let expr = parser(self)?;
            elements.push(expr);
        }

        exit_parse!(self);
        Ok(elements)
    }

    // token helpers

    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn peek_kind(&mut self) -> Option<&TokenKind> {
        self.peek().map(|token| &token.kind)
    }

    fn expect(&mut self, kind: TokenKind) -> ParseResult<Token> {
        if let Some(token) = self.next() {
            if token.kind == kind {
                Ok(token)
            } else if let TokenKind::Error(err) = token.kind {
                Err(*err)
            } else {
                Err(CompilerError::new(
                    token.span,
                    format!("expected `{:?}`, received `{:?}`", kind, token.kind),
                ))
            }
        } else {
            Err(CompilerError::new(
                Span::dummy(),
                format!("reached EOF expecting `{:?}`", kind),
            ))
        }
    }
}

impl CompilerError {
    fn eof(message: &str) -> Self {
        Self {
            // todo: don't
            span: Span::dummy(),
            message: format!("reached EOF while parsing `{}`", message),
            note: None,
        }
    }
}
