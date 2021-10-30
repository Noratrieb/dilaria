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
        self.expect(TokenType::Semi);
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
        todo!()
    }

    fn term(&mut self) -> ParseResult<'code, Expr> {
        todo!()
    }

    fn factor(&mut self) -> ParseResult<'code, Expr> {
        todo!()
    }

    fn unary(&mut self) -> ParseResult<'code, Expr> {
        todo!()
    }

    fn primary(&mut self) -> ParseResult<'code, Expr> {
        match self.next().ok_or(ParseErr::EOF)?.kind {
            TokenType::String(literal) => Ok(Expr::Literal(Literal::String(literal))),
            TokenType::Number(literal) => Ok(Expr::Literal(Literal::Number(literal))),
            TokenType::False => Ok(Expr::Literal(Literal::Boolean(false))),
            TokenType::True => Ok(Expr::Literal(Literal::Boolean(true))),
            TokenType::Null => Ok(Expr::Literal(Literal::Null)),
            TokenType::BraceO => todo!(),
            TokenType::BracketO => todo!(),
            TokenType::ParenO => todo!(),
            _ => todo!(),
        }
    }

    fn object_literal(&mut self) -> ParseResult<'code, Expr> {
        self.expect(TokenType::BraceO)?;
        self.expect(TokenType::BraceC)?;
        Ok(Expr::Literal(Literal::Object))
    }

    fn array_literal(&mut self) -> ParseResult<'code, Expr> {
        todo!()
    }

    // helpers

    fn next(&mut self) -> Option<Token<'code>> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<&Token<'code>> {
        self.tokens.peek()
    }

    fn expect(&mut self, kind: TokenType<'code>) -> ParseResult<'code, ()> {
        if let Some(token) = self.next() {
            if token.kind == kind {
                Ok(())
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
