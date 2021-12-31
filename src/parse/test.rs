//! Test for the parser
//!
//! These tests are horrible and break all the time. Never do it like this again.
//! That said it's too late to fix it.

use crate::errors::Span;
use crate::parse::Parser;
use crate::RtAlloc;
use bumpalo::Bump;
use prelude::*;

mod prelude {
    pub(super) use super::{parser, rt, test_literal_bin_op, test_number_literal, token};
    pub(super) use crate::ast::{Expr, Stmt};
    pub(super) use crate::lex::TokenKind::*;
    pub type Token = crate::lex::Token;
    pub type TokenType = crate::lex::TokenKind;
    pub(super) use bumpalo::Bump;
}

fn token(kind: TokenType) -> Token {
    Token {
        span: Span::dummy(),
        kind,
    }
}

fn rt() -> RtAlloc {
    // SAFETY: this is just a test what could go wrong
    unsafe { RtAlloc::new() }
}

fn parser(tokens: std::vec::Vec<Token>, alloc: &Bump) -> Parser<std::vec::IntoIter<Token>>
where {
    Parser {
        tokens: tokens.into_iter().peekable(),
        depth: 0,
        inside_fn_depth: 0,
        inside_loop_depth: 0,
        bump: alloc,
    }
}

fn test_literal_bin_op<F: FnOnce(Vec<Token>, &Bump) -> Expr>(token_type: TokenType, parser: F) {
    let tokens = [Number(10.0), token_type, Number(4.0)].map(token).into();

    let alloc = Bump::new();
    let ast = parser(tokens, &alloc);
    insta::assert_debug_snapshot!(ast);
}

fn test_number_literal<F: FnOnce(Vec<Token>, &Bump) -> Expr>(parser: F) {
    let tokens = [Number(10.0)].map(token).into();

    let alloc = Bump::new();
    let ast = parser(tokens, &alloc);
    insta::assert_debug_snapshot!(ast);
}

mod assignment {
    use super::prelude::*;
    use crate::parse::test::rt;
    use bumpalo::Bump;

    fn parse_assignment(tokens: Vec<Token>, alloc: &Bump) -> Stmt {
        let mut parser = parser(tokens, alloc);
        parser.assignment().unwrap()
    }

    #[test]
    fn simple() {
        let mut rt = rt();
        let tokens = [Ident(rt.intern_string("hugo")), Equal, Number(10.0), Semi]
            .map(token)
            .into();

        let alloc = Bump::new();
        let ast = parse_assignment(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn call_expr() {
        let mut rt = rt();
        let tokens = [
            Ident(rt.intern_string("hugo")),
            Dot,
            Ident(rt.intern_string("age")),
            Equal,
            Number(2021.0),
            Minus,
            Number(1986.0),
            Semi,
        ]
        .map(token)
        .into();

        let alloc = Bump::new();
        let ast = parse_assignment(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }
}

mod r#fn {
    use super::prelude::*;

    fn parse_fn(tokens: Vec<Token>, alloc: &Bump) -> Stmt {
        let mut parser = parser(tokens, alloc);
        parser.fn_decl().unwrap()
    }

    #[test]
    fn empty() {
        let mut rt = rt();
        let tokens = [
            Fn,
            Ident(rt.intern_string("empty")),
            ParenO,
            ParenC,
            BraceO,
            BraceC,
        ]
        .map(token)
        .into();

        let alloc = Bump::new();
        let ast = parse_fn(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn params_body() {
        let mut rt = rt();
        let tokens = [
            Fn,
            Ident(rt.intern_string("empty")),
            ParenO,
            Ident(rt.intern_string("a")),
            Comma,
            Ident(rt.intern_string("b")),
            ParenC,
            BraceO,
            Number(10.0),
            Plus,
            Number(20.0),
            Semi,
            BraceC,
        ]
        .map(token)
        .into();
        let alloc = Bump::new();
        let ast = parse_fn(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }
}

mod r#if {
    use super::prelude::*;
    use crate::ast::IfStmt;

    fn parse_if(tokens: Vec<Token>, alloc: &Bump) -> IfStmt {
        let mut parser = parser(tokens, alloc);
        parser.if_stmt().unwrap()
    }

    #[test]
    fn empty() {
        let tokens = [If, True, BraceO, BraceC].map(token).into();
        let alloc = Bump::new();
        let ast = parse_if(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn if_else() {
        let tokens = [If, True, BraceO, BraceC, Else, BraceO, BraceC]
            .map(token)
            .into();
        let alloc = Bump::new();
        let ast = parse_if(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn if_else_if() {
        let tokens = [If, True, BraceO, BraceC, Else, If, True, BraceO, BraceC]
            .map(token)
            .into();
        let alloc = Bump::new();
        let ast = parse_if(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn if_else_if_else() {
        let tokens = [
            If, True, BraceO, BraceC, Else, If, True, BraceO, BraceC, Else, BraceO, BraceC,
        ]
        .map(token)
        .into();
        let alloc = Bump::new();
        let ast = parse_if(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }
}

mod print {
    use super::prelude::*;

    fn parse_print(tokens: Vec<Token>, alloc: &Bump) -> Stmt {
        let mut parser = parser(tokens, alloc);
        parser.print_stmt().unwrap()
    }

    #[test]
    fn print_true() {
        let tokens = [Print, True, Semi].map(token).into();
        let alloc = Bump::new();
        let ast = parse_print(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }
}

mod r#while {
    use super::prelude::*;

    fn parse_while(tokens: Vec<Token>, alloc: &Bump) -> Stmt {
        let mut parser = parser(tokens, alloc);
        parser.while_stmt().unwrap()
    }

    #[test]
    fn empty() {
        let tokens = [While, True, BraceO, BraceC].map(token).into();
        let alloc = Bump::new();
        let ast = parse_while(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn or_condition_break() {
        let tokens = [While, False, Or, True, BraceO, Break, Semi, BraceC]
            .map(token)
            .into();
        let alloc = Bump::new();
        let ast = parse_while(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }
}

mod r#loop {
    use super::prelude::*;

    fn parse_loop(tokens: Vec<Token>, alloc: &Bump) -> Stmt {
        let mut parser = parser(tokens, alloc);
        parser.loop_stmt().unwrap()
    }

    #[test]
    fn empty() {
        let tokens = [Loop, BraceO, BraceC].map(token).into();
        let alloc = Bump::new();
        let ast = parse_loop(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn with_break() {
        let tokens = [Loop, BraceO, Break, Semi, BraceC].map(token).into();
        let alloc = Bump::new();
        let ast = parse_loop(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn break_after_inner() {
        let tokens = [Loop, BraceO, Loop, BraceO, BraceC, Break, Semi, BraceC]
            .map(token)
            .into();
        let alloc = Bump::new();
        let ast = parse_loop(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }
}

mod block {
    use super::prelude::*;
    use crate::ast::Block;

    fn parse_block(tokens: Vec<Token>, alloc: &Bump) -> Block {
        let mut parser = parser(tokens, alloc);
        parser.block().unwrap()
    }

    #[test]
    fn empty() {
        let tokens = [BraceO, BraceC].map(token).into();
        let alloc = Bump::new();
        let ast = parse_block(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn two_expressions() {
        let tokens = [BraceO, Number(10.0), Semi, Number(20.0), Semi, BraceC]
            .map(token)
            .into();
        let alloc = Bump::new();
        let ast = parse_block(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn nested() {
        let tokens = [BraceO, BraceO, BraceC, BraceC].map(token).into();
        let alloc = Bump::new();
        let ast = parse_block(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }
}

mod expr {
    use super::prelude::*;

    fn parse_expr(tokens: Vec<Token>, alloc: &Bump) -> Expr {
        let mut parser = parser(tokens, alloc);
        parser.expression().unwrap()
    }

    #[test]
    fn stack_overflow() {
        let tokens = std::iter::repeat(BracketO)
            .map(token)
            .take(100_000)
            .collect();

        let alloc = Bump::new();
        let ast = parser(tokens, &alloc).expression();
        assert!(ast.is_err());
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_expr);
    }

    #[test]
    fn add_multiply() {
        let tokens = [Number(10.0), Plus, Number(20.0), Asterisk, Number(100.0)]
            .map(token)
            .into();
        let alloc = Bump::new();
        let ast = parse_expr(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn equal_unary() {
        let tokens = [Number(10.0), EqualEqual, Minus, Number(10.0)]
            .map(token)
            .into();
        let alloc = Bump::new();
        let ast = parse_expr(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn parentheses_mul_add() {
        let tokens = [
            Number(10.0),
            Asterisk,
            ParenO,
            Number(20.0),
            Plus,
            Number(30.0),
            ParenC,
        ]
        .map(token)
        .into();
        let alloc = Bump::new();
        let ast = parse_expr(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }
}

mod logical_or {
    use super::prelude::*;

    fn parse_logical_or(tokens: Vec<Token>, alloc: &Bump) -> Expr {
        let mut parser = parser(tokens, alloc);
        parser.logical_or().unwrap()
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_logical_or);
    }

    #[test]
    fn or() {
        test_literal_bin_op(Or, parse_logical_or);
    }
}

mod logical_and {
    use super::prelude::*;

    fn parse_logical_and(tokens: Vec<Token>, alloc: &Bump) -> Expr {
        let mut parser = parser(tokens, alloc);
        parser.logical_and().unwrap()
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_logical_and);
    }

    #[test]
    fn and() {
        test_literal_bin_op(And, parse_logical_and);
    }
}

mod equality {
    use super::prelude::*;

    fn parse_equality(tokens: Vec<Token>, alloc: &Bump) -> Expr {
        let mut parser = parser(tokens, alloc);
        parser.equality().unwrap()
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_equality);
    }

    #[test]
    fn not_equal() {
        test_literal_bin_op(BangEqual, parse_equality);
    }

    #[test]
    fn equal() {
        test_literal_bin_op(EqualEqual, parse_equality);
    }
}

mod comparison {
    use super::prelude::*;

    fn parse_comparison(tokens: Vec<Token>, alloc: &Bump) -> Expr {
        let mut parser = parser(tokens, alloc);
        parser.comparison().unwrap()
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_comparison);
    }

    #[test]
    fn greater() {
        test_literal_bin_op(Greater, parse_comparison);
    }

    #[test]
    fn greater_equal() {
        test_literal_bin_op(GreaterEqual, parse_comparison);
    }

    #[test]
    fn less() {
        test_literal_bin_op(Less, parse_comparison);
    }

    #[test]
    fn less_equal() {
        test_literal_bin_op(LessEqual, parse_comparison);
    }
}

mod term {
    use super::prelude::*;

    fn parse_term(tokens: Vec<Token>, alloc: &Bump) -> Expr {
        let mut parser = parser(tokens, alloc);
        parser.term().unwrap()
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_term);
    }

    #[test]
    fn add() {
        test_literal_bin_op(Plus, parse_term);
    }

    #[test]
    fn sub() {
        test_literal_bin_op(Minus, parse_term);
    }
}

mod factor {
    use super::prelude::*;

    fn parse_factor(tokens: Vec<Token>, alloc: &Bump) -> Expr {
        let mut parser = parser(tokens, alloc);
        parser.factor().unwrap()
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_factor);
    }

    #[test]
    fn multiply() {
        test_literal_bin_op(Asterisk, parse_factor);
    }

    #[test]
    fn divide() {
        test_literal_bin_op(Slash, parse_factor);
    }

    #[test]
    fn modulo() {
        test_literal_bin_op(Percent, parse_factor);
    }
}

mod unary {
    use super::prelude::*;

    fn parse_unary(tokens: Vec<Token>, alloc: &Bump) -> Expr {
        let mut parser = parser(tokens, alloc);
        parser.unary().unwrap()
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_unary);
    }

    #[test]
    fn not() {
        let tokens = [Not, True].map(token).into();
        let alloc = Bump::new();
        let ast = parse_unary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn neg() {
        let tokens = [Minus, Number(10.0)].map(token).into();
        let alloc = Bump::new();
        let ast = parse_unary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }
}

mod call {
    use super::prelude::*;

    fn parse_call(tokens: Vec<Token>, alloc: &Bump) -> Expr {
        let mut parser = parser(tokens, alloc);
        parser.call().unwrap()
    }

    #[test]
    fn field_simple() {
        let mut rt = rt();
        let tokens = [
            Ident(rt.intern_string("hugo")),
            Dot,
            Ident(rt.intern_string("name")),
        ]
        .map(token)
        .into();
        let alloc = Bump::new();
        let ast = parse_call(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn simple() {
        let mut rt = rt();
        let tokens = [Ident(rt.intern_string("print")), ParenO, ParenC]
            .map(token)
            .into();
        let alloc = Bump::new();
        let ast = parse_call(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn fn_args() {
        let mut rt = rt();
        let tokens = [
            Ident(rt.intern_string("print")),
            ParenO,
            Number(10.0),
            Comma,
            Number(5.0),
            Comma,
            ParenC,
        ]
        .map(token)
        .into();
        let alloc = Bump::new();
        let ast = parse_call(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn nested() {
        let mut rt = rt();
        let tokens = [
            Ident(rt.intern_string("hugo")),
            Dot,
            Ident(rt.intern_string("name")),
            Dot,
            Ident(rt.intern_string("print")),
            ParenO,
            ParenC,
        ]
        .map(token)
        .into();
        let alloc = Bump::new();
        let ast = parse_call(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn with_exprs() {
        let mut rt = rt();
        // print((10 + 5).abs())
        let tokens = [
            Ident(rt.intern_string("print")),
            ParenO,
            ParenO,
            Number(10.0),
            Plus,
            Number(5.0),
            ParenC,
            Dot,
            Ident(rt.intern_string("abs")),
            ParenO,
            ParenC,
            ParenC,
        ]
        .map(token)
        .into();
        let alloc = Bump::new();
        let ast = parse_call(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }
}

mod primary {
    use super::prelude::*;

    fn parse_primary(tokens: Vec<Token>, alloc: &Bump) -> Expr {
        let mut parser = parser(tokens, alloc);
        parser.primary().unwrap()
    }

    #[test]
    fn ident_test() {
        let mut rt = rt();
        let tokens = [Ident(rt.intern_string("tokens"))].map(token).into();
        let alloc = Bump::new();
        let ast = parse_primary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn string() {
        let tokens = [Number(10.0)].map(token).into();
        let alloc = Bump::new();
        let ast = parse_primary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn number() {
        let mut rt = rt();
        let tokens = [String(rt.intern_string("uwu"))].map(token).into();
        let alloc = Bump::new();
        let ast = parse_primary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn empty_object() {
        let tokens = [BraceO, BraceC].map(token).into();
        let alloc = Bump::new();
        let ast = parse_primary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn empty_array() {
        let tokens = [BracketO, BracketC].map(token).into();
        let alloc = Bump::new();
        let ast = parse_primary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn r#false() {
        let tokens = [False].map(token).into();
        let alloc = Bump::new();
        let ast = parse_primary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn r#true() {
        let tokens = [True].map(token).into();
        let alloc = Bump::new();
        let ast = parse_primary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn null() {
        let tokens = [Null].map(token).into();
        let alloc = Bump::new();
        let ast = parse_primary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn empty_array_literal() {
        let tokens = [BracketO, BracketC].map(token).into();
        let alloc = Bump::new();
        let ast = parse_primary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn single_array_literal() {
        let tokens = [BracketO, Number(10.0), BracketC].map(token).into();
        let alloc = Bump::new();
        let ast = parse_primary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn single_array_literal_trailing_comma() {
        let tokens = [BracketO, Number(10.0), Comma, BracketC].map(token).into();
        let alloc = Bump::new();
        let ast = parse_primary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn two_array_literal() {
        let tokens = [BracketO, Number(10.0), Comma, Number(10.0), BracketC]
            .map(token)
            .into();
        let alloc = Bump::new();
        let ast = parse_primary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn two_array_literal_trailing_comma() {
        let tokens = [BracketO, Number(10.0), Comma, Number(10.0), Comma, BracketC]
            .map(token)
            .into();
        let alloc = Bump::new();
        let ast = parse_primary(tokens, &alloc);
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn two_array_literal_no_comma() {
        let tokens = [BracketO, Number(10.0), Number(10.0), BracketC]
            .map(token)
            .into();

        let alloc = Bump::new();
        let mut parser = parser(tokens, &alloc);
        let expr = parser.primary();
        assert!(expr.is_err());
    }
}
