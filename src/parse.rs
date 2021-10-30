#![allow(dead_code)]

use crate::ast::*;
use crate::errors::{CompilerError, Span};
use crate::lex::{Token, TokenType};
use std::iter::Peekable;

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseErr> {
    let mut parser = Parser {
        tokens: tokens.into_iter().peekable(),
    };
    let program = parser.program()?;
    Ok(program)
}

#[derive(Debug)]
struct Parser<'code> {
    tokens: Peekable<std::vec::IntoIter<Token<'code>>>,
}

type ParseResult<'code, T> = Result<T, ParseErr<'code>>;

impl<'code> Parser<'code> {
    fn program(&mut self) -> ParseResult<'code, Program> {
        Ok(Program(self.block()?))
    }

    fn block(&mut self) -> ParseResult<'code, Block> {
        let mut stmts = Vec::new();
        loop {
            if let Some(Token {
                kind: TokenType::BraceC,
                ..
            }) = self.peek()
            {
                let _ = self.next();
                return Ok(Block(stmts));
            }
            let stmt = self.statement()?;
            stmts.push(stmt);
        }
    }

    fn statement(&mut self) -> ParseResult<'code, Stmt> {
        let expr = self.expression()?;
        self.expect(TokenType::Semi)?;
        Ok(Stmt::Expr(expr))
    }

    fn declaration(&mut self) -> ParseResult<'code, Declaration> {
        todo!()
    }

    fn assignment(&mut self) -> ParseResult<'code, Assignment> {
        todo!()
    }

    fn fn_decl(&mut self) -> ParseResult<'code, FnDecl> {
        todo!()
    }

    fn if_stmt(&mut self) -> ParseResult<'code, IfStmt> {
        todo!()
    }

    fn loop_stmt(&mut self) -> ParseResult<'code, Block> {
        todo!()
    }

    fn expression(&mut self) -> ParseResult<'code, Expr> {
        todo!()
    }

    fn logical_or(&mut self) -> ParseResult<'code, Expr> {
        todo!()
    }

    fn logical_and(&mut self) -> ParseResult<'code, Expr> {
        todo!()
    }

    fn equality(&mut self) -> ParseResult<'code, Expr> {
        todo!()
    }

    fn comparison(&mut self) -> ParseResult<'code, Expr> {
        let lhs = self.term()?;
        match self.peek().map(|token| &token.kind) {
            Some(TokenType::GreaterThan) => {
                let _ = self.next();
                let rhs = self.term()?;
                self.binary_op(lhs, BinaryOpKind::Greater, rhs)
            }
            Some(TokenType::GreaterThanEqual) => {
                let _ = self.next();
                let rhs = self.term()?;
                self.binary_op(lhs, BinaryOpKind::GreaterEqual, rhs)
            }
            Some(TokenType::LessThan) => {
                let _ = self.next();
                let rhs = self.term()?;
                self.binary_op(lhs, BinaryOpKind::Less, rhs)
            }
            Some(TokenType::LessThanEqual) => {
                let _ = self.next();
                let rhs = self.term()?;
                self.binary_op(lhs, BinaryOpKind::LessEqual, rhs)
            }
            _ => Ok(lhs),
        }
    }

    fn term(&mut self) -> ParseResult<'code, Expr> {
        let lhs = self.factor()?;
        match self.peek().map(|token| &token.kind) {
            Some(TokenType::Plus) => {
                let _ = self.next();
                let rhs = self.factor()?;
                self.binary_op(lhs, BinaryOpKind::Add, rhs)
            }
            Some(TokenType::Minus) => {
                let _ = self.next();
                let rhs = self.factor()?;
                self.binary_op(lhs, BinaryOpKind::Sub, rhs)
            }
            _ => Ok(lhs),
        }
    }

    fn factor(&mut self) -> ParseResult<'code, Expr> {
        let lhs = self.unary()?;
        match self.peek().map(|token| &token.kind) {
            Some(TokenType::Asterisk) => {
                let _ = self.next();
                let rhs = self.unary()?;
                self.binary_op(lhs, BinaryOpKind::Mul, rhs)
            }
            Some(TokenType::Slash) => {
                let _ = self.next();
                let rhs = self.unary()?;
                self.binary_op(lhs, BinaryOpKind::Div, rhs)
            }
            Some(TokenType::Percent) => {
                let _ = self.next();
                let rhs = self.unary()?;
                self.binary_op(lhs, BinaryOpKind::Mod, rhs)
            }
            _ => Ok(lhs),
        }
    }

    fn unary(&mut self) -> ParseResult<'code, Expr> {
        match self.peek().map(|token| &token.kind) {
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
            Some(_) => self.primary(),
            None => todo!(),
        }
    }

    fn primary(&mut self) -> ParseResult<'code, Expr> {
        let next = self.next().ok_or(ParseErr::EOF)?;
        match next.kind {
            TokenType::String(literal) => Ok(Expr::Literal(Literal::String(literal, next.span))),
            TokenType::Number(literal) => Ok(Expr::Literal(Literal::Number(literal, next.span))),
            TokenType::False => Ok(Expr::Literal(Literal::Boolean(false, next.span))),
            TokenType::True => Ok(Expr::Literal(Literal::Boolean(true, next.span))),
            TokenType::Null => Ok(Expr::Literal(Literal::Null(next.span))),
            TokenType::BraceO => {
                self.expect(TokenType::BraceC)?;
                Ok(Expr::Literal(Literal::Object(next.span)))
            }
            TokenType::BracketO => {
                let mut elements = Vec::new();
                while self.peek().ok_or(ParseErr::EOF)?.kind != TokenType::BracketC {
                    let expr = self.expression()?;
                    elements.push(expr);
                    self.expect(TokenType::Comma)?;
                }
                let closing_bracket = self.expect(TokenType::BracketC)?;
                Ok(Expr::Literal(Literal::Array(
                    elements,
                    next.span.extend(closing_bracket.span),
                )))
            }
            TokenType::ParenO => todo!(),
            _ => todo!(),
        }
    }

    fn object_literal(&mut self) -> ParseResult<'code, Expr> {
        let open_span = self.expect(TokenType::BraceO)?.span;
        let close_span = self.expect(TokenType::BraceC)?.span;
        Ok(Expr::Literal(Literal::Object(open_span.extend(close_span))))
    }

    fn array_literal(&mut self) -> ParseResult<'code, Expr> {
        todo!()
    }

    // other helpers

    fn binary_op(&mut self, lhs: Expr, kind: BinaryOpKind, rhs: Expr) -> ParseResult<'code, Expr> {
        Ok(Expr::BinaryOp(Box::new(BinaryOp {
            span: lhs.span().extend(rhs.span()),
            lhs,
            rhs,
            kind,
        })))
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

    fn expect(&mut self, kind: TokenType<'code>) -> ParseResult<'code, Token> {
        if let Some(token) = self.next() {
            if token.kind == kind {
                Ok(token)
            } else {
                Err(ParseErr::MismatchedKind { expected: kind })
            }
        } else {
            Err(ParseErr::EOF)
        }
    }
}

#[derive(Debug)]
pub enum ParseErr<'code> {
    MismatchedKind { expected: TokenType<'code> },
    InvalidToken(TokenType<'code>),
    EOF,
}

impl CompilerError for ParseErr<'_> {
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

#[cfg(test)]
mod test {
    use crate::lex::{Token, TokenType};
    use crate::parse::Parser;

    mod prelude {
        pub(super) use super::{parser, token};
        pub(super) use crate::ast::{Expr, Literal};
        pub(super) use crate::errors::Span;
        pub(super) use crate::lex::{Token, TokenType};
    }

    fn token(kind: TokenType) -> Token {
        Token {
            span: crate::errors::Span::dummy(),
            kind,
        }
    }

    fn parser<'a, T: Into<Vec<Token<'a>>>>(tokens: T) -> Parser<'a> {
        Parser {
            tokens: tokens.into().into_iter().peekable(),
        }
    }

    mod factor {
        use super::prelude::*;
        use crate::ast::{BinaryOp, BinaryOpKind};

        fn parse_factor<'a, T: Into<Vec<Token<'a>>>>(tokens: T) -> Expr {
            let mut parser = parser(tokens);
            parser.factor().unwrap()
        }

        fn test_literal_factor(token_type: TokenType, expected_op_kind: BinaryOpKind) {
            let tokens = [TokenType::Number(10.0), token_type, TokenType::Number(4.0)].map(token);
            let factor = parse_factor(tokens);
            assert_eq!(
                Expr::BinaryOp(Box::new(BinaryOp {
                    span: Span::dummy(),
                    lhs: Expr::Literal(Literal::Number(10.0, Span::dummy())),
                    rhs: Expr::Literal(Literal::Number(4.0, Span::dummy())),
                    kind: expected_op_kind
                })),
                factor
            );
        }

        #[test]
        fn multiply() {
            test_literal_factor(TokenType::Asterisk, BinaryOpKind::Mul);
        }

        #[test]
        fn divide() {
            test_literal_factor(TokenType::Slash, BinaryOpKind::Div);
        }

        #[test]
        fn modulo() {
            test_literal_factor(TokenType::Percent, BinaryOpKind::Mod);
        }
    }

    mod unary {
        use super::prelude::*;

        fn parse_unary<'a, T: Into<Vec<Token<'a>>>>(tokens: T) -> Expr {
            let mut parser = parser(tokens);
            parser.primary().unwrap()
        }

        #[test]
        fn number_literal() {
            let tokens = [TokenType::Number(10.0)].map(token);
            let unary = parse_unary(tokens);
            assert_eq!(Expr::Literal(Literal::Number(10.0, Span::dummy())), unary);
        }
    }

    mod primary {
        use super::prelude::*;

        fn parse_primary<'a, T: Into<Vec<Token<'a>>>>(tokens: T) -> Expr {
            let mut parser = parser(tokens);
            parser.primary().unwrap()
        }

        #[test]
        fn string() {
            let tokens = [TokenType::Number(10.0)].map(token);
            let literal = parse_primary(tokens);
            assert_eq!(Expr::Literal(Literal::Number(10.0, Span::dummy())), literal);
        }

        #[test]
        fn number() {
            let tokens = [TokenType::String("uwu".to_string())].map(token);
            let literal = parse_primary(tokens);
            assert_eq!(
                Expr::Literal(Literal::String("uwu".to_string(), Span::dummy())),
                literal
            );
        }

        #[test]
        fn empty_object() {
            let tokens = [TokenType::BraceO, TokenType::BraceC].map(token);
            let literal = parse_primary(tokens);
            assert_eq!(Expr::Literal(Literal::Object(Span::dummy())), literal);
        }

        #[test]
        fn empty_array() {
            let tokens = [TokenType::BracketO, TokenType::BracketC].map(token);
            let literal = parse_primary(tokens);
            assert_eq!(
                Expr::Literal(Literal::Array(Vec::new(), Span::dummy())),
                literal
            );
        }

        #[test]
        fn r#false() {
            let tokens = [TokenType::False].map(token);
            let literal = parse_primary(tokens);
            assert_eq!(
                Expr::Literal(Literal::Boolean(false, Span::dummy())),
                literal
            );
        }

        #[test]
        fn r#true() {
            let tokens = [TokenType::True].map(token);
            let literal = parse_primary(tokens);
            assert_eq!(
                Expr::Literal(Literal::Boolean(true, Span::dummy())),
                literal
            );
        }

        #[test]
        fn null() {
            let tokens = [TokenType::Null].map(token);
            let literal = parse_primary(tokens);
            assert_eq!(Expr::Literal(Literal::Null(Span::dummy())), literal);
        }
    }
}
