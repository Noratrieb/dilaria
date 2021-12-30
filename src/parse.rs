#[cfg(test)]
mod test;

use crate::ast::*;
use crate::errors::{CompilerError, Span};
use crate::lex::{Token, TokenType};
use bumpalo::boxed::Box;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use std::iter::Peekable;

#[derive(Debug)]
struct Parser<'code, 'ast, I>
where
    I: Iterator<Item = Result<Token<'code>, CompilerError>>,
    I: 'code,
{
    tokens: Peekable<I>,
    depth: usize,
    inside_fn_depth: usize,
    inside_loop_depth: usize,
    bump: &'ast Bump,
}

pub fn parse<'ast, 'code>(
    tokens: impl Iterator<Item = Result<Token<'code>, CompilerError>> + 'code,
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

type ParseResult<'code, T> = Result<T, ParseErr<'code>>;

macro_rules! parse_bin_op {
    ($self: ident, $lhs: ident, $kind: expr, $function: ident) => {{
        let _ = $self.next();
        let rhs = $self.$function()?;
        Ok(Expr::BinaryOp(Box::new_in(
            BinaryOp {
                span: $lhs.span().extend(rhs.span()),
                lhs: $lhs,
                rhs,
                kind: $kind,
            },
            $self.bump,
        )))
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

impl<'code, 'ast, I> Parser<'code, 'ast, I>
where
    I: Iterator<Item = Result<Token<'code>, CompilerError>>,
    I: 'code,
{
    const MAX_DEPTH: usize = 100;

    fn program(&mut self) -> ParseResult<'code, Program<'ast>> {
        Ok(Program(self.statement_list()?))
    }

    fn too_nested_error(&mut self) -> ParseResult<'code, ()> {
        let next_token = self.next()?;
        match next_token {
            Some(token) => Err(ParseErr::MaxDepth(token.span)),
            None => Err(ParseErr::Eof("reached EOF while being nested to deeply")),
        }
    }

    fn statement_list(&mut self) -> ParseResult<'code, Vec<'ast, Stmt<'ast>>> {
        enter_parse!(self);
        let mut stmts = Vec::new_in(self.bump);
        let return_stmts = loop {
            if let Some(TokenType::BraceC) | None = self.peek_kind()? {
                break Ok(stmts);
            }
            let stmt = self.statement()?;
            stmts.push(stmt);
        };
        exit_parse!(self);
        return_stmts
    }

    fn block(&mut self) -> ParseResult<'code, Block<'ast>> {
        enter_parse!(self);

        let start_span = self.expect(TokenType::BraceO)?.span;
        let stmts = self.statement_list()?;
        let end_span = self.expect(TokenType::BraceC)?.span;

        exit_parse!(self);

        Ok(Block {
            stmts,
            span: start_span.extend(end_span),
        })
    }

    fn statement(&mut self) -> ParseResult<'code, Stmt<'ast>> {
        enter_parse!(self);

        let stmt = match *self.peek_kind()?.ok_or(ParseErr::Eof("statement"))? {
            TokenType::Let => self.declaration(),
            TokenType::Fn => self.fn_decl(),
            TokenType::If => Ok(Stmt::If(self.if_stmt()?)),
            TokenType::Loop => self.loop_stmt(),
            TokenType::While => self.while_stmt(),
            TokenType::Break => self.break_stmt(),
            TokenType::Return => self.return_stmt(),
            TokenType::Print => self.print_stmt(),
            TokenType::BraceO => Ok(Stmt::Block(self.block()?)),
            _ => {
                let stmt = self.assignment()?;
                Ok(stmt)
            }
        };
        exit_parse!(self);
        stmt
    }

    fn declaration(&mut self) -> ParseResult<'code, Stmt<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenType::Let)?.span;
        let name = self.ident()?;
        self.expect(TokenType::Equal)?;
        let init = self.expression()?;
        self.expect(TokenType::Semi)?;

        exit_parse!(self);

        Ok(Stmt::Declaration(Declaration {
            span: keyword_span.extend(init.span()),
            name,
            init,
        }))
    }

    fn fn_decl(&mut self) -> ParseResult<'code, Stmt<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenType::Fn)?.span;
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

    fn fn_args(&mut self) -> ParseResult<'code, Vec<'ast, Ident>> {
        enter_parse!(self);

        self.expect(TokenType::ParenO)?;
        let params = self.parse_list(TokenType::ParenC, Self::ident)?;
        self.expect(TokenType::ParenC)?;

        exit_parse!(self);

        Ok(params)
    }

    fn if_stmt(&mut self) -> ParseResult<'code, IfStmt<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenType::If)?.span;
        let cond = self.expression()?;
        let body = self.block()?;

        let else_part = if let Some(TokenType::Else) = self.peek_kind()? {
            Some(self.else_part()?)
        } else {
            None
        };

        exit_parse!(self);

        Ok(IfStmt {
            span: keyword_span
                .extend(body.span)
                .option_extend(else_part.as_ref().map(|part| part.span())),
            cond,
            body,
            else_part: else_part.map(|part| Box::new_in(part, self.bump)),
        })
    }

    fn else_part(&mut self) -> ParseResult<'code, ElsePart<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenType::Else)?.span;

        let else_part = if let Some(TokenType::If) = self.peek_kind()? {
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

    fn loop_stmt(&mut self) -> ParseResult<'code, Stmt<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenType::Loop)?.span;

        self.inside_loop_depth += 1;
        let block = self.block()?;
        self.inside_loop_depth -= 1;

        let loop_span = keyword_span.extend(block.span);

        exit_parse!(self);

        Ok(Stmt::Loop(block, keyword_span.extend(loop_span)))
    }

    fn while_stmt(&mut self) -> ParseResult<'code, Stmt<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenType::While)?.span;
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

    fn break_stmt(&mut self) -> ParseResult<'code, Stmt<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenType::Break)?.span;
        let semi_span = self.expect(TokenType::Semi)?.span;

        exit_parse!(self);

        if self.inside_loop_depth == 0 {
            Err(ParseErr::BreakOutsideLoop(keyword_span.extend(semi_span)))
        } else {
            Ok(Stmt::Break(keyword_span.extend(semi_span)))
        }
    }

    fn return_stmt(&mut self) -> ParseResult<'code, Stmt<'ast>> {
        enter_parse!(self);

        let keyword_span = self.expect(TokenType::Return)?.span;

        let expr = if let Some(TokenType::Semi) = self.peek_kind()? {
            None
        } else {
            Some(self.expression()?)
        };

        let semi_span = self.expect(TokenType::Semi)?.span;

        exit_parse!(self);

        if self.inside_fn_depth == 0 {
            Err(ParseErr::ReturnOutsideFunction(
                keyword_span.extend(semi_span),
            ))
        } else {
            Ok(Stmt::Return(expr, keyword_span.extend(semi_span)))
        }
    }

    fn print_stmt(&mut self) -> ParseResult<'code, Stmt<'ast>> {
        enter_parse!(self);

        let print_span = self.expect(TokenType::Print)?.span;

        let expr = self.expression()?;

        let semi_span = self.expect(TokenType::Semi)?.span;

        exit_parse!(self);

        Ok(Stmt::Print(expr, print_span.extend(semi_span)))
    }

    fn assignment(&mut self) -> ParseResult<'code, Stmt<'ast>> {
        enter_parse!(self);

        let expr = self.expression()?;

        let stmt = if let Some(TokenType::Equal) = self.peek_kind()? {
            let _ = self.expect(TokenType::Equal)?;
            let init = self.expression()?;
            let semi_span = self.expect(TokenType::Semi)?.span;
            Ok(Stmt::Assignment(Assignment {
                span: expr.span().extend(semi_span),
                lhs: expr,
                rhs: init,
            }))
        } else {
            let _ = self.expect(TokenType::Semi)?;
            Ok(Stmt::Expr(expr))
        };

        exit_parse!(self);
        stmt
    }

    fn expression(&mut self) -> ParseResult<'code, Expr<'ast>> {
        enter_parse!(self);
        let return_expr = self.logical_or();
        exit_parse!(self);
        return_expr
    }

    fn logical_or(&mut self) -> ParseResult<'code, Expr<'ast>> {
        enter_parse!(self);

        let lhs = self.logical_and()?;
        let return_expr = match self.peek_kind()? {
            Some(TokenType::Or) => parse_bin_op!(self, lhs, BinaryOpKind::Or, logical_or),
            _ => Ok(lhs),
        };

        exit_parse!(self);
        return_expr
    }

    fn logical_and(&mut self) -> ParseResult<'code, Expr<'ast>> {
        enter_parse!(self);

        let lhs = self.equality()?;
        let return_expr = match self.peek_kind()? {
            Some(TokenType::And) => parse_bin_op!(self, lhs, BinaryOpKind::And, logical_and),
            _ => Ok(lhs),
        };

        exit_parse!(self);
        return_expr
    }

    fn equality(&mut self) -> ParseResult<'code, Expr<'ast>> {
        enter_parse!(self);

        let lhs = self.comparison()?;
        let return_expr = match self.peek_kind()? {
            Some(TokenType::BangEqual) => {
                parse_bin_op!(self, lhs, BinaryOpKind::NotEqual, comparison)
            }
            Some(TokenType::EqualEqual) => {
                parse_bin_op!(self, lhs, BinaryOpKind::Equal, comparison)
            }
            _ => Ok(lhs),
        };
        exit_parse!(self);
        return_expr
    }

    fn comparison(&mut self) -> ParseResult<'code, Expr<'ast>> {
        enter_parse!(self);

        let lhs = self.term()?;
        let return_expr = match self.peek_kind()? {
            Some(TokenType::Greater) => parse_bin_op!(self, lhs, BinaryOpKind::Greater, term),
            Some(TokenType::GreaterEqual) => {
                parse_bin_op!(self, lhs, BinaryOpKind::GreaterEqual, term)
            }
            Some(TokenType::Less) => parse_bin_op!(self, lhs, BinaryOpKind::Less, term),
            Some(TokenType::LessEqual) => {
                parse_bin_op!(self, lhs, BinaryOpKind::LessEqual, term)
            }
            _ => Ok(lhs),
        };
        exit_parse!(self);
        return_expr
    }

    fn term(&mut self) -> ParseResult<'code, Expr<'ast>> {
        enter_parse!(self);

        let lhs = self.factor()?;
        let return_expr = match self.peek_kind()? {
            Some(TokenType::Plus) => parse_bin_op!(self, lhs, BinaryOpKind::Add, term),
            Some(TokenType::Minus) => parse_bin_op!(self, lhs, BinaryOpKind::Sub, term),
            _ => Ok(lhs),
        };
        exit_parse!(self);
        return_expr
    }

    fn factor(&mut self) -> ParseResult<'code, Expr<'ast>> {
        enter_parse!(self);

        let lhs = self.unary()?;
        let return_expr = match self.peek_kind()? {
            Some(TokenType::Asterisk) => parse_bin_op!(self, lhs, BinaryOpKind::Mul, factor),
            Some(TokenType::Slash) => parse_bin_op!(self, lhs, BinaryOpKind::Div, factor),
            Some(TokenType::Percent) => parse_bin_op!(self, lhs, BinaryOpKind::Mod, factor),
            _ => Ok(lhs),
        };
        exit_parse!(self);
        return_expr
    }

    fn unary(&mut self) -> ParseResult<'code, Expr<'ast>> {
        enter_parse!(self);

        let return_expr = match self.peek_kind()? {
            Some(TokenType::Not) => {
                let unary_op_span = self.next()?.unwrap().span;
                let expr = self.call()?;
                Ok(Expr::UnaryOp(Box::new_in(
                    UnaryOp {
                        span: unary_op_span.extend(expr.span()),
                        expr,
                        kind: UnaryOpKind::Not,
                    },
                    self.bump,
                )))
            }
            Some(TokenType::Minus) => {
                let unary_op_span = self.next()?.unwrap().span;
                let expr = self.call()?;
                Ok(Expr::UnaryOp(Box::new_in(
                    UnaryOp {
                        span: unary_op_span.extend(expr.span()),
                        expr,
                        kind: UnaryOpKind::Neg,
                    },
                    self.bump,
                )))
            }
            _ => self.call(),
        };
        exit_parse!(self);
        return_expr
    }

    fn call(&mut self) -> ParseResult<'code, Expr<'ast>> {
        enter_parse!(self);

        let mut expr = self.primary()?;

        loop {
            expr = match self.peek_kind()? {
                Some(TokenType::ParenO) => {
                    let open_span = self.expect(TokenType::ParenO)?.span;
                    let args = self.parse_list(TokenType::ParenC, Self::expression)?;
                    let close_span = self.expect(TokenType::ParenC)?.span;

                    Expr::Call(Box::new_in(
                        Call {
                            callee: expr,
                            span: open_span.extend(close_span),
                            kind: CallKind::Fn(args),
                        },
                        self.bump,
                    ))
                }
                Some(TokenType::Dot) => {
                    let dot_span = self.expect(TokenType::Dot)?.span;
                    let field = self.ident()?;

                    Expr::Call(Box::new_in(
                        Call {
                            callee: expr,
                            span: dot_span.extend(field.span),
                            kind: CallKind::Field(field),
                        },
                        self.bump,
                    ))
                }
                _ => break,
            }
        }

        exit_parse!(self);

        Ok(expr)
    }

    fn primary(&mut self) -> ParseResult<'code, Expr<'ast>> {
        enter_parse!(self);

        let next = self.next()?.ok_or(ParseErr::Eof("primary"))?;
        let return_expr = match next.kind {
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
                Ok(Expr::Ident(Ident {
                    sym: name_owned,
                    span: next.span,
                }))
            }
            _ => Err(ParseErr::InvalidTokenPrimary(next)),
        };
        exit_parse!(self);
        return_expr
    }

    fn ident(&mut self) -> ParseResult<'code, Ident> {
        enter_parse!(self);

        let Token { kind, span } = self.next()?.ok_or(ParseErr::Eof("identifier"))?;
        let return_expr = match kind {
            TokenType::Ident(name) => {
                let name_owned = name.to_owned();
                Ok(Ident {
                    sym: name_owned,
                    span,
                })
            }
            _ => {
                return Err(ParseErr::MismatchedKind {
                    expected: TokenType::Ident("<ident>"),
                    actual: Token { span, kind },
                })
            }
        };
        exit_parse!(self);
        return_expr
    }

    fn object_literal(&mut self, open_span: Span) -> ParseResult<'code, Expr<'ast>> {
        enter_parse!(self);

        let close_span = self.expect(TokenType::BraceC)?.span;

        exit_parse!(self);
        Ok(Expr::Literal(Literal::Object(open_span.extend(close_span))))
    }

    fn array_literal(&mut self, open_span: Span) -> ParseResult<'code, Expr<'ast>> {
        enter_parse!(self);

        let elements = self.parse_list(TokenType::BracketC, Self::expression)?;
        let closing_bracket = self.expect(TokenType::BracketC)?;

        let return_expr = Ok(Expr::Literal(Literal::Array(
            elements,
            open_span.extend(closing_bracket.span),
        )));
        exit_parse!(self);
        return_expr
    }

    fn parse_list<T, F>(
        &mut self,
        close: TokenType<'code>,
        mut parser: F,
    ) -> ParseResult<'code, Vec<'ast, T>>
    where
        F: FnMut(&mut Self) -> ParseResult<'code, T>,
    {
        enter_parse!(self);

        let mut elements = Vec::new_in(self.bump);

        if self.peek_kind()? == Some(&close) {
            return Ok(elements);
        }

        let expr = parser(self)?;
        elements.push(expr);

        while self
            .peek_kind()?
            .ok_or_else(|| ParseErr::EofExpecting(close.clone()))?
            != &close
        {
            self.expect(TokenType::Comma)?;

            // trailing comma support
            if self.peek_kind()? == Some(&close) {
                break;
            }

            let expr = parser(self)?;
            elements.push(expr);
        }

        exit_parse!(self);
        Ok(elements)
    }

    // token helpers

    fn next(&mut self) -> ParseResult<'code, Option<Token<'code>>> {
        match self.tokens.next() {
            Some(Ok(t)) => Ok(Some(t)),
            Some(Err(comp_err)) => Err(ParseErr::LexError(comp_err)),
            None => Ok(None),
        }
    }

    fn peek(&mut self) -> ParseResult<'code, Option<&Token<'code>>> {
        match self.tokens.peek() {
            Some(Ok(t)) => Ok(Some(t)),
            Some(Err(comp_err)) => Err(ParseErr::LexError(comp_err.clone())),
            None => Ok(None),
        }
    }

    fn peek_kind(&mut self) -> ParseResult<'code, Option<&TokenType<'code>>> {
        self.peek().map(|option| option.map(|token| &token.kind))
    }

    fn expect(&mut self, kind: TokenType<'code>) -> ParseResult<'code, Token> {
        if let Some(token) = self.next()? {
            if token.kind == kind {
                Ok(token)
            } else {
                Err(ParseErr::MismatchedKind {
                    expected: kind,
                    actual: token,
                })
            }
        } else {
            Err(ParseErr::EofExpecting(kind))
        }
    }
}

#[derive(Debug)]
pub enum ParseErr<'code> {
    MaxDepth(Span),
    BreakOutsideLoop(Span),
    ReturnOutsideFunction(Span),
    MismatchedKind {
        expected: TokenType<'code>,
        actual: Token<'code>,
    },
    InvalidTokenPrimary(Token<'code>),
    EofExpecting(TokenType<'code>),
    Eof(&'static str),
    LexError(CompilerError),
}

// todo: remove this and ParseErr
impl From<ParseErr<'_>> for CompilerError {
    fn from(error: ParseErr<'_>) -> Self {
        Self {
            span: match &error {
                ParseErr::MismatchedKind {
                    actual: Token { span, .. },
                    ..
                } => *span,
                ParseErr::InvalidTokenPrimary(Token { span, .. }) => *span,
                ParseErr::EofExpecting(_) => Span::dummy(),
                ParseErr::Eof(_) => Span::dummy(),
                ParseErr::BreakOutsideLoop(span) => *span,
                ParseErr::ReturnOutsideFunction(span) => *span,
                ParseErr::MaxDepth(span) => *span,
                ParseErr::LexError(err) => err.span,
            },
            message: match &error {
                ParseErr::MismatchedKind { expected, actual } => {
                    format!("expected `{:?}`, received `{:?}`", expected, actual.kind)
                }
                ParseErr::InvalidTokenPrimary(token) => {
                    format!("invalid token in expression: `{:?}`", token.kind)
                }
                ParseErr::EofExpecting(token) => {
                    format!("reached EOF searching for `{:?}`", token)
                }
                ParseErr::Eof(message) => {
                    format!("reached EOF while parsing `{}`", message)
                }
                ParseErr::BreakOutsideLoop(_) => "break used outside of loop".to_string(),
                ParseErr::ReturnOutsideFunction(_) => "return used outside of function".to_string(),
                ParseErr::MaxDepth(_) => "reached maximal nesting depth".to_string(),
                ParseErr::LexError(err) => err.message.clone(),
            },
            note: match error {
                ParseErr::LexError(err) => err.note.clone(),
                _ => None,
            },
        }
    }
}
