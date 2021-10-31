#![allow(dead_code)]

#[cfg(test)]
mod test;

use crate::ast::*;
use crate::errors::{CompilerError, Span};
use crate::lex::{Token, TokenType};
use std::iter::Peekable;

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseErr> {
    let mut parser = Parser {
        tokens: tokens.into_iter().peekable(),
        inside_fn_depth: 0,
        inside_loop_depth: 0,
    };
    let program = parser.program()?;
    Ok(program)
}

#[derive(Debug)]
struct Parser<'code> {
    tokens: Peekable<std::vec::IntoIter<Token<'code>>>,
    inside_fn_depth: usize,
    inside_loop_depth: usize,
}

type ParseResult<'code, T> = Result<T, ParseErr<'code>>;

macro_rules! parse_bin_op {
    ($self: ident, $lhs: ident, $kind: expr, $function: ident) => {{
        let _ = $self.next();
        let rhs = $self.$function()?;
        Ok(Expr::BinaryOp(Box::new(BinaryOp {
            span: $lhs.span().extend(rhs.span()),
            lhs: $lhs,
            rhs,
            kind: $kind,
        })))
    }};
}

impl<'code> Parser<'code> {
    fn program(&mut self) -> ParseResult<'code, Program> {
        Ok(Program(self.statement_list()?))
    }

    fn statement_list(&mut self) -> ParseResult<'code, Vec<Stmt>> {
        let mut stmts = Vec::new();
        loop {
            if let Some(TokenType::BraceC) | None = self.peek_kind() {
                let _ = self.next();
                return Ok(stmts);
            }
            let stmt = self.statement()?;
            stmts.push(stmt);
        }
    }

    fn block(&mut self) -> ParseResult<'code, Block> {
        let start_span = self.expect(TokenType::BraceO)?.span;
        let stmts = self.statement_list()?;
        let end_span = self.expect(TokenType::BraceC)?.span;
        Ok(Block {
            stmts,
            span: start_span.extend(end_span),
        })
    }

    fn statement(&mut self) -> ParseResult<'code, Stmt> {
        match self.peek_kind().ok_or(ParseErr::EOF("statement"))? {
            &TokenType::Let => self.declaration(),
            &TokenType::Fn => self.fn_decl(),
            &TokenType::If => Ok(Stmt::If(self.if_stmt()?)),
            &TokenType::Loop => self.loop_stmt(),
            &TokenType::While => self.while_stmt(),
            &TokenType::Break => self.break_stmt(),
            &TokenType::Return => self.return_stmt(),
            &TokenType::BraceO => Ok(Stmt::Block(self.block()?)),
            _ => {
                let expr = self.expression()?;
                self.expect(TokenType::Semi)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn declaration(&mut self) -> ParseResult<'code, Stmt> {
        todo!()
    }

    fn assignment(&mut self) -> ParseResult<'code, Stmt> {
        todo!()
    }

    fn fn_decl(&mut self) -> ParseResult<'code, Stmt> {
        todo!()
    }

    fn if_stmt(&mut self) -> ParseResult<'code, IfStmt> {
        let keyword_span = self.expect(TokenType::If)?.span;
        let cond = self.expression()?;
        let body = self.block()?;

        let else_part = if let Some(TokenType::Else) = self.peek_kind() {
            Some(self.else_part()?)
        } else {
            None
        };

        Ok(IfStmt {
            span: keyword_span
                .extend(body.span)
                .option_extend(else_part.as_ref().map(|part| part.span())),
            cond,
            body,
            else_part: else_part.map(Box::new),
        })
    }

    fn else_part(&mut self) -> ParseResult<'code, ElsePart> {
        let keyword_span = self.expect(TokenType::Else)?.span;

        if let Some(TokenType::If) = self.peek_kind() {
            let else_if_stmt = self.if_stmt()?;
            let else_span = keyword_span.extend(else_if_stmt.span);
            Ok(ElsePart::ElseIf(else_if_stmt, else_span))
        } else {
            let block = self.block()?;
            let else_span = keyword_span.extend(block.span);
            Ok(ElsePart::Else(block, else_span))
        }
    }

    fn loop_stmt(&mut self) -> ParseResult<'code, Stmt> {
        let keyword_span = self.expect(TokenType::Loop)?.span;

        self.inside_loop_depth += 1;
        let block = self.block()?;
        self.inside_loop_depth -= 1;

        let loop_span = keyword_span.extend(block.span);
        Ok(Stmt::Loop(block, keyword_span.extend(loop_span)))
    }

    fn while_stmt(&mut self) -> ParseResult<'code, Stmt> {
        let keyword_span = self.expect(TokenType::While)?.span;
        let cond = self.expression()?;
        let body = self.block()?;
        Ok(Stmt::While(WhileStmt {
            span: keyword_span.extend(body.span),
            cond,
            body,
        }))
    }

    fn break_stmt(&mut self) -> ParseResult<'code, Stmt> {
        let keyword_span = self.expect(TokenType::Break)?.span;
        let semi_span = self.expect(TokenType::Semi)?.span;

        if self.inside_loop_depth == 0 {
            Err(ParseErr::BreakOutsideLoop(keyword_span.extend(semi_span)))
        } else {
            Ok(Stmt::Break(keyword_span.extend(semi_span)))
        }
    }

    fn return_stmt(&mut self) -> ParseResult<'code, Stmt> {
        let keyword_span = self.expect(TokenType::Return)?.span;

        let expr = if let Some(TokenType::Semi) = self.peek_kind() {
            None
        } else {
            Some(self.expression()?)
        };

        let semi_span = self.expect(TokenType::Semi)?.span;

        if self.inside_fn_depth == 0 {
            Err(ParseErr::ReturnOutsideFunction(
                keyword_span.extend(semi_span),
            ))
        } else {
            Ok(Stmt::Return(expr, keyword_span.extend(semi_span)))
        }
    }

    fn expression(&mut self) -> ParseResult<'code, Expr> {
        self.logical_or()
    }

    fn logical_or(&mut self) -> ParseResult<'code, Expr> {
        let lhs = self.logical_and()?;
        match self.peek_kind() {
            Some(TokenType::Or) => parse_bin_op!(self, lhs, BinaryOpKind::Or, logical_and),
            _ => Ok(lhs),
        }
    }

    fn logical_and(&mut self) -> ParseResult<'code, Expr> {
        let lhs = self.equality()?;
        match self.peek_kind() {
            Some(TokenType::And) => parse_bin_op!(self, lhs, BinaryOpKind::And, equality),
            _ => Ok(lhs),
        }
    }

    fn equality(&mut self) -> ParseResult<'code, Expr> {
        let lhs = self.comparison()?;
        match self.peek_kind() {
            Some(TokenType::BangEqual) => {
                parse_bin_op!(self, lhs, BinaryOpKind::NotEqual, comparison)
            }
            Some(TokenType::EqualEqual) => {
                parse_bin_op!(self, lhs, BinaryOpKind::Equal, comparison)
            }
            _ => Ok(lhs),
        }
    }

    fn comparison(&mut self) -> ParseResult<'code, Expr> {
        let lhs = self.term()?;
        match self.peek_kind() {
            Some(TokenType::Greater) => parse_bin_op!(self, lhs, BinaryOpKind::Greater, term),
            Some(TokenType::GreaterEqual) => {
                parse_bin_op!(self, lhs, BinaryOpKind::GreaterEqual, term)
            }
            Some(TokenType::Less) => parse_bin_op!(self, lhs, BinaryOpKind::Less, term),
            Some(TokenType::LessEqual) => {
                parse_bin_op!(self, lhs, BinaryOpKind::LessEqual, term)
            }
            _ => Ok(lhs),
        }
    }

    fn term(&mut self) -> ParseResult<'code, Expr> {
        let lhs = self.factor()?;
        match self.peek_kind() {
            Some(TokenType::Plus) => parse_bin_op!(self, lhs, BinaryOpKind::Add, factor),
            Some(TokenType::Minus) => parse_bin_op!(self, lhs, BinaryOpKind::Sub, factor),
            _ => Ok(lhs),
        }
    }

    fn factor(&mut self) -> ParseResult<'code, Expr> {
        let lhs = self.unary()?;
        match self.peek_kind() {
            Some(TokenType::Asterisk) => parse_bin_op!(self, lhs, BinaryOpKind::Mul, unary),
            Some(TokenType::Slash) => parse_bin_op!(self, lhs, BinaryOpKind::Div, unary),
            Some(TokenType::Percent) => parse_bin_op!(self, lhs, BinaryOpKind::Mod, unary),
            _ => Ok(lhs),
        }
    }

    fn unary(&mut self) -> ParseResult<'code, Expr> {
        match self.peek_kind() {
            Some(TokenType::Not) => {
                let unary_op_span = self.next().unwrap().span;
                let expr = self.expression()?;
                Ok(Expr::UnaryOp(Box::new(UnaryOp {
                    span: unary_op_span.extend(expr.span()),
                    expr,
                    kind: UnaryOpKind::Not,
                })))
            }
            Some(TokenType::Minus) => {
                let unary_op_span = self.next().unwrap().span;
                let expr = self.expression()?;
                Ok(Expr::UnaryOp(Box::new(UnaryOp {
                    span: unary_op_span.extend(expr.span()),
                    expr,
                    kind: UnaryOpKind::Neg,
                })))
            }
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> ParseResult<'code, Expr> {
        let next = self.next().ok_or(ParseErr::EOF("primary"))?;
        match next.kind {
            TokenType::String(literal) => Ok(Expr::Literal(Literal::String(literal, next.span))),
            TokenType::Number(literal) => Ok(Expr::Literal(Literal::Number(literal, next.span))),
            TokenType::False => Ok(Expr::Literal(Literal::Boolean(false, next.span))),
            TokenType::True => Ok(Expr::Literal(Literal::Boolean(true, next.span))),
            TokenType::Null => Ok(Expr::Literal(Literal::Null(next.span))),
            TokenType::BraceO => self.object_literal(next.span),
            TokenType::BracketO => self.array_literal(next.span),
            TokenType::ParenO => {
                let expr = self.expression()?;
                let _ = self.expect(TokenType::ParenC)?;
                Ok(expr)
            }
            TokenType::Ident(name) => {
                let name_owned = name.to_owned();
                Ok(Expr::Ident(name_owned, next.span))
            }
            _ => Err(ParseErr::InvalidTokenPrimary(next)),
        }
    }

    fn object_literal(&mut self, open_span: Span) -> ParseResult<'code, Expr> {
        let close_span = self.expect(TokenType::BraceC)?.span;
        Ok(Expr::Literal(Literal::Object(open_span.extend(close_span))))
    }

    fn array_literal(&mut self, open_span: Span) -> ParseResult<'code, Expr> {
        let mut elements = Vec::new();
        while self
            .peek()
            .ok_or(ParseErr::EOFExpecting(TokenType::BracketC))?
            .kind
            != TokenType::BracketC
        {
            let expr = self.expression()?;
            elements.push(expr);
            self.expect(TokenType::Comma)?;
        }
        let closing_bracket = self.expect(TokenType::BracketC)?;
        Ok(Expr::Literal(Literal::Array(
            elements,
            open_span.extend(closing_bracket.span),
        )))
    }

    // token helpers

    #[must_use]
    fn next(&mut self) -> Option<Token<'code>> {
        self.tokens.next()
    }

    #[must_use]
    fn peek(&mut self) -> Option<&Token<'code>> {
        self.tokens.peek()
    }

    #[must_use]
    fn peek_kind(&mut self) -> Option<&TokenType<'code>> {
        self.peek().map(|token| &token.kind)
    }

    fn expect(&mut self, kind: TokenType<'code>) -> ParseResult<'code, Token> {
        if let Some(token) = self.next() {
            if token.kind == kind {
                Ok(token)
            } else {
                Err(ParseErr::MismatchedKind {
                    expected: kind,
                    actual: token,
                })
            }
        } else {
            Err(ParseErr::EOFExpecting(kind))
        }
    }
}

#[derive(Debug)]
pub enum ParseErr<'code> {
    BreakOutsideLoop(Span),
    ReturnOutsideFunction(Span),
    MismatchedKind {
        expected: TokenType<'code>,
        actual: Token<'code>,
    },
    InvalidTokenPrimary(Token<'code>),
    EOFExpecting(TokenType<'code>),
    EOF(&'static str),
}

impl CompilerError for ParseErr<'_> {
    fn span(&self) -> Span {
        match self {
            ParseErr::MismatchedKind {
                actual: Token { span, .. },
                ..
            } => *span,
            ParseErr::InvalidTokenPrimary(Token { span, .. }) => *span,
            ParseErr::EOFExpecting(_) => Span::dummy(),
            ParseErr::EOF(_) => Span::dummy(),
            ParseErr::BreakOutsideLoop(span) => *span,
            ParseErr::ReturnOutsideFunction(span) => *span,
        }
    }

    fn message(&self) -> String {
        match self {
            ParseErr::MismatchedKind { expected, actual } => {
                format!("expected: {:?}, received: {:?}", expected, actual.kind)
            }
            ParseErr::InvalidTokenPrimary(token) => {
                format!("invalid token in expression: {:?}", token.kind)
            }
            ParseErr::EOFExpecting(token) => {
                format!("reached EOF searching for: {:?}", token)
            }
            ParseErr::EOF(message) => {
                format!("reached EOF while parsing: {}", message)
            }
            ParseErr::BreakOutsideLoop(_) => "break used outside of loop".to_string(),
            ParseErr::ReturnOutsideFunction(_) => "return used outside of function".to_string(),
        }
    }

    fn note(&self) -> Option<String> {
        None
    }
}
