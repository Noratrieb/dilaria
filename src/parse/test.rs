use crate::ast::BinaryOp;
use crate::parse::Parser;
use prelude::*;

mod prelude {
    pub(super) use super::{
        empty_block, ident, num_lit, parser, test_literal_bin_op, test_number_literal, token,
    };
    pub(super) use crate::ast::*;
    pub(super) use crate::errors::Span;
    pub(super) use crate::lex::{
        Token,
        TokenType::{self, *},
    };
}

fn token(kind: TokenType) -> Token {
    Token {
        span: Span::dummy(),
        kind,
    }
}

fn num_lit(number: f64) -> Expr {
    Expr::Literal(Literal::Number(number, Span::dummy()))
}

fn ident(name: &str) -> Ident {
    Ident {
        name: name.to_string(),
        span: Default::default(),
    }
}

fn empty_block() -> Block {
    Block {
        stmts: vec![],
        span: Span::dummy(),
    }
}

fn parser(tokens: Vec<Token>) -> Parser {
    Parser {
        tokens: tokens.into_iter().peekable(),
        inside_fn_depth: 0,
        inside_loop_depth: 0,
    }
}

fn test_literal_bin_op<F: FnOnce(Vec<Token<'_>>) -> Expr>(
    token_type: TokenType,
    expected_op_kind: BinaryOpKind,
    parser: F,
) {
    let tokens = [TokenType::Number(10.0), token_type, TokenType::Number(4.0)]
        .map(token)
        .into();
    let factor = parser(tokens);
    assert_eq!(
        Expr::BinaryOp(Box::new(BinaryOp {
            span: Span::dummy(),
            lhs: num_lit(10.0),
            rhs: num_lit(4.0),
            kind: expected_op_kind
        })),
        factor
    );
}

fn test_number_literal<F: FnOnce(Vec<Token<'_>>) -> Expr>(parser: F) {
    let tokens = [TokenType::Number(10.0)].map(token).into();
    let unary = parser(tokens);
    assert_eq!(num_lit(10.0), unary);
}

mod r#fn {
    use super::prelude::*;

    fn parse_fn(tokens: Vec<Token>) -> Stmt {
        let mut parser = parser(tokens);
        parser.fn_decl().unwrap()
    }

    #[test]
    fn empty() {
        let tokens = [Fn, Ident("empty"), ParenO, ParenC, BraceO, BraceC]
            .map(token)
            .into();
        let ast = parse_fn(tokens);
        assert_eq!(
            Stmt::FnDecl(FnDecl {
                span: Span::dummy(),
                name: ident("empty"),
                params: vec![],
                body: Block {
                    stmts: vec![],
                    span: Default::default()
                }
            }),
            ast
        );
    }

    #[test]
    fn params_body() {
        let tokens = [
            Fn,
            Ident("empty"),
            ParenO,
            Ident("a"),
            Comma,
            Ident("b"),
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
        let ast = parse_fn(tokens);
        assert_eq!(
            Stmt::FnDecl(FnDecl {
                span: Span::dummy(),
                name: ident("empty"),
                params: vec![ident("a"), ident("b")],
                body: Block {
                    stmts: vec![Stmt::Expr(Expr::BinaryOp(Box::new(BinaryOp {
                        span: Default::default(),
                        lhs: num_lit(10.0),
                        rhs: num_lit(20.0),
                        kind: BinaryOpKind::Add,
                    })))],
                    span: Default::default()
                }
            }),
            ast
        );
    }
}

mod r#if {
    use super::prelude::*;

    fn parse_if(tokens: Vec<Token>) -> IfStmt {
        let mut parser = parser(tokens);
        parser.if_stmt().unwrap()
    }

    #[test]
    fn empty() {
        let tokens = [If, True, BraceO, BraceC].map(token).into();
        let ast = parse_if(tokens);
        assert_eq!(
            IfStmt {
                span: Span::dummy(),
                cond: Expr::Literal(Literal::Boolean(true, Span::dummy())),
                body: empty_block(),
                else_part: None
            },
            ast
        );
    }

    #[test]
    fn if_else() {
        let tokens = [If, True, BraceO, BraceC, Else, BraceO, BraceC]
            .map(token)
            .into();
        let ast = parse_if(tokens);
        assert_eq!(
            IfStmt {
                span: Span::dummy(),
                cond: Expr::Literal(Literal::Boolean(true, Span::dummy())),
                body: empty_block(),
                else_part: Some(Box::new(ElsePart::Else(empty_block(), Span::dummy())))
            },
            ast
        );
    }

    #[test]
    fn if_else_if() {
        let tokens = [If, True, BraceO, BraceC, Else, If, True, BraceO, BraceC]
            .map(token)
            .into();
        let ast = parse_if(tokens);
        assert_eq!(
            IfStmt {
                span: Span::dummy(),
                cond: Expr::Literal(Literal::Boolean(true, Span::dummy())),
                body: empty_block(),
                else_part: Some(Box::new(ElsePart::ElseIf(
                    IfStmt {
                        span: Span::dummy(),
                        cond: Expr::Literal(Literal::Boolean(true, Span::dummy())),
                        body: empty_block(),
                        else_part: None
                    },
                    Span::dummy()
                )))
            },
            ast
        );
    }

    #[test]
    fn if_else_if_else() {
        let tokens = [
            If, True, BraceO, BraceC, Else, If, True, BraceO, BraceC, Else, BraceO, BraceC,
        ]
        .map(token)
        .into();
        let ast = parse_if(tokens);
        assert_eq!(
            IfStmt {
                span: Span::dummy(),
                cond: Expr::Literal(Literal::Boolean(true, Span::dummy())),
                body: empty_block(),
                else_part: Some(Box::new(ElsePart::ElseIf(
                    IfStmt {
                        span: Span::dummy(),
                        cond: Expr::Literal(Literal::Boolean(true, Span::dummy())),
                        body: empty_block(),
                        else_part: Some(Box::new(ElsePart::Else(empty_block(), Span::dummy())))
                    },
                    Span::dummy()
                )))
            },
            ast
        );
    }
}

mod r#while {
    use super::prelude::*;

    fn parse_while(tokens: Vec<Token>) -> Stmt {
        let mut parser = parser(tokens);
        parser.while_stmt().unwrap()
    }

    #[test]
    fn empty() {
        let tokens = [While, True, BraceO, BraceC].map(token).into();
        let ast = parse_while(tokens);
        assert_eq!(
            Stmt::While(WhileStmt {
                span: Span::dummy(),
                cond: Expr::Literal(Literal::Boolean(true, Span::dummy())),
                body: empty_block()
            }),
            ast
        );
    }

    #[test]
    fn or_condition_break() {
        let tokens = [While, False, Or, True, BraceO, Break, Semi, BraceC]
            .map(token)
            .into();
        let ast = parse_while(tokens);
        assert_eq!(
            Stmt::While(WhileStmt {
                span: Span::dummy(),
                cond: Expr::BinaryOp(Box::new(BinaryOp {
                    span: Span::dummy(),
                    lhs: Expr::Literal(Literal::Boolean(false, Span::dummy())),
                    rhs: Expr::Literal(Literal::Boolean(true, Span::dummy())),
                    kind: BinaryOpKind::Or
                })),
                body: Block {
                    stmts: vec![Stmt::Break(Span::dummy())],
                    span: Span::dummy()
                }
            }),
            ast
        );
    }
}

mod r#loop {
    use super::prelude::*;

    fn parse_loop(tokens: Vec<Token>) -> Stmt {
        let mut parser = parser(tokens);
        parser.loop_stmt().unwrap()
    }

    #[test]
    fn empty() {
        let tokens = [Loop, BraceO, BraceC].map(token).into();
        let ast = parse_loop(tokens);
        assert_eq!(Stmt::Loop(empty_block(), Span::dummy()), ast);
    }

    #[test]
    fn with_break() {
        let tokens = [Loop, BraceO, Break, Semi, BraceC].map(token).into();
        let ast = parse_loop(tokens);
        assert_eq!(
            Stmt::Loop(
                Block {
                    stmts: vec![Stmt::Break(Span::dummy())],
                    span: Default::default()
                },
                Span::dummy()
            ),
            ast
        );
    }

    #[test]
    fn break_after_inner() {
        let tokens = [Loop, BraceO, Loop, BraceO, BraceC, Break, Semi, BraceC]
            .map(token)
            .into();
        let ast = parse_loop(tokens);
        assert_eq!(
            Stmt::Loop(
                Block {
                    stmts: vec![
                        Stmt::Loop(empty_block(), Span::dummy()),
                        Stmt::Break(Span::dummy())
                    ],
                    span: Span::dummy()
                },
                Span::dummy()
            ),
            ast
        );
    }
}

mod block {
    use super::prelude::*;

    fn parse_block(tokens: Vec<Token>) -> Block {
        let mut parser = parser(tokens);
        parser.block().unwrap()
    }

    #[test]
    fn empty() {
        let tokens = [BraceO, BraceC].map(token).into();
        let ast = parse_block(tokens);
        assert_eq!(empty_block(), ast);
    }

    #[test]
    fn two_expressions() {
        let tokens = [BraceO, Number(10.0), Semi, Number(20.0), Semi, BraceC]
            .map(token)
            .into();
        let ast = parse_block(tokens);
        assert_eq!(
            Block {
                stmts: vec![Stmt::Expr(num_lit(10.0)), Stmt::Expr(num_lit(20.0)),],
                span: Span::dummy()
            },
            ast
        );
    }

    #[test]
    fn nested() {
        let tokens = [BraceO, BraceO, BraceC, BraceC].map(token).into();
        let ast = parse_block(tokens);
        assert_eq!(
            Block {
                stmts: vec![Stmt::Block(empty_block())],
                span: Span::dummy()
            },
            ast
        );
    }
}

mod expr {
    use super::prelude::*;
    use crate::ast::{UnaryOp, UnaryOpKind};

    fn parse_expr(tokens: Vec<Token>) -> Expr {
        let mut parser = parser(tokens);
        parser.expression().unwrap()
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
        let expr = parse_expr(tokens);
        assert_eq!(
            Expr::BinaryOp(Box::new(BinaryOp {
                span: Span::dummy(),
                lhs: num_lit(10.0),
                rhs: Expr::BinaryOp(Box::new(BinaryOp {
                    span: Span::dummy(),
                    lhs: num_lit(20.0),
                    rhs: num_lit(100.0),

                    kind: BinaryOpKind::Mul
                })),
                kind: BinaryOpKind::Add
            })),
            expr
        );
    }

    #[test]
    fn equal_unary() {
        let tokens = [Number(10.0), EqualEqual, Minus, Number(10.0)]
            .map(token)
            .into();
        let expr = parse_expr(tokens);
        assert_eq!(
            Expr::BinaryOp(Box::new(BinaryOp {
                span: Span::dummy(),
                lhs: num_lit(10.0),
                rhs: Expr::UnaryOp(Box::new(UnaryOp {
                    span: Span::dummy(),
                    expr: num_lit(10.0),
                    kind: UnaryOpKind::Neg
                })),
                kind: BinaryOpKind::Equal
            })),
            expr
        );
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
        let expr = parse_expr(tokens);
        assert_eq!(
            Expr::BinaryOp(Box::new(BinaryOp {
                span: Span::dummy(),
                lhs: num_lit(10.0),
                rhs: Expr::BinaryOp(Box::new(BinaryOp {
                    span: Span::dummy(),
                    lhs: num_lit(20.0),
                    rhs: num_lit(30.0),

                    kind: BinaryOpKind::Add
                })),
                kind: BinaryOpKind::Mul
            })),
            expr
        );
    }
}

mod logical_or {
    use super::prelude::*;

    fn parse_logical_or(tokens: Vec<Token>) -> Expr {
        let mut parser = parser(tokens);
        parser.logical_or().unwrap()
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_logical_or);
    }

    #[test]
    fn and() {
        test_literal_bin_op(Or, BinaryOpKind::Or, parse_logical_or);
    }
}

mod logical_and {
    use super::prelude::*;

    fn parse_logical_and(tokens: Vec<Token>) -> Expr {
        let mut parser = parser(tokens);
        parser.logical_and().unwrap()
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_logical_and);
    }

    #[test]
    fn and() {
        test_literal_bin_op(And, BinaryOpKind::And, parse_logical_and);
    }
}

mod equality {
    use super::prelude::*;

    fn parse_equality(tokens: Vec<Token>) -> Expr {
        let mut parser = parser(tokens);
        parser.equality().unwrap()
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_equality);
    }

    #[test]
    fn not_equal() {
        test_literal_bin_op(BangEqual, BinaryOpKind::NotEqual, parse_equality);
    }

    #[test]
    fn equal() {
        test_literal_bin_op(EqualEqual, BinaryOpKind::Equal, parse_equality);
    }
}

mod comparison {
    use super::prelude::*;

    fn parse_comparison(tokens: Vec<Token>) -> Expr {
        let mut parser = parser(tokens);
        parser.comparison().unwrap()
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_comparison);
    }

    #[test]
    fn greater() {
        test_literal_bin_op(Greater, BinaryOpKind::Greater, parse_comparison);
    }

    #[test]
    fn greater_equal() {
        test_literal_bin_op(GreaterEqual, BinaryOpKind::GreaterEqual, parse_comparison);
    }

    #[test]
    fn less() {
        test_literal_bin_op(Less, BinaryOpKind::Less, parse_comparison);
    }

    #[test]
    fn less_equal() {
        test_literal_bin_op(LessEqual, BinaryOpKind::LessEqual, parse_comparison);
    }
}

mod term {
    use super::prelude::*;

    fn parse_term(tokens: Vec<Token>) -> Expr {
        let mut parser = parser(tokens);
        parser.term().unwrap()
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_term);
    }

    #[test]
    fn add() {
        test_literal_bin_op(Plus, BinaryOpKind::Add, parse_term);
    }

    #[test]
    fn sub() {
        test_literal_bin_op(Minus, BinaryOpKind::Sub, parse_term);
    }
}

mod factor {
    use super::prelude::*;

    fn parse_factor(tokens: Vec<Token>) -> Expr {
        let mut parser = parser(tokens);
        parser.factor().unwrap()
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_factor);
    }

    #[test]
    fn multiply() {
        test_literal_bin_op(Asterisk, BinaryOpKind::Mul, parse_factor);
    }

    #[test]
    fn divide() {
        test_literal_bin_op(Slash, BinaryOpKind::Div, parse_factor);
    }

    #[test]
    fn modulo() {
        test_literal_bin_op(Percent, BinaryOpKind::Mod, parse_factor);
    }
}

mod unary {
    use super::prelude::*;
    use crate::ast::{UnaryOp, UnaryOpKind};

    fn parse_unary(tokens: Vec<Token>) -> Expr {
        let mut parser = parser(tokens);
        parser.unary().unwrap()
    }

    #[test]
    fn number_literal() {
        test_number_literal(parse_unary);
    }

    // needs expr support

    #[test]
    fn not() {
        let tokens = [Not, True].map(token).into();
        let unary = parse_unary(tokens);
        assert_eq!(
            Expr::UnaryOp(Box::new(UnaryOp {
                span: Span::dummy(),
                expr: Expr::Literal(Literal::Boolean(true, Span::dummy())),
                kind: UnaryOpKind::Not
            })),
            unary
        );
    }

    #[test]
    fn neg() {
        let tokens = [Minus, Number(10.0)].map(token).into();
        let unary = parse_unary(tokens);
        assert_eq!(
            Expr::UnaryOp(Box::new(UnaryOp {
                span: Span::dummy(),
                expr: num_lit(10.0),
                kind: UnaryOpKind::Neg
            })),
            unary
        );
    }
}

mod call {
    use super::prelude::*;

    fn parse_call(tokens: Vec<Token>) -> Expr {
        let mut parser = parser(tokens);
        parser.call().unwrap()
    }

    #[test]
    fn field_simple() {
        let tokens = [Ident("hugo"), Dot, Ident("name")].map(token).into();
        let literal = parse_call(tokens);
        assert_eq!(
            Expr::Call(Box::new(Call {
                callee: Expr::Ident(ident("hugo")),
                span: Default::default(),
                kind: CallKind::Field(ident("name"))
            })),
            literal
        );
    }

    #[test]
    fn simple() {
        let tokens = [Ident("print"), ParenO, ParenC].map(token).into();
        let literal = parse_call(tokens);
        assert_eq!(
            Expr::Call(Box::new(Call {
                callee: Expr::Ident(ident("print")),
                span: Default::default(),
                kind: CallKind::Fn(Vec::new())
            })),
            literal
        );
    }

    #[test]
    fn fn_args() {
        let tokens = [
            Ident("print"),
            ParenO,
            Number(10.0),
            Comma,
            Number(5.0),
            Comma,
            ParenC,
        ]
        .map(token)
        .into();
        let literal = parse_call(tokens);
        assert_eq!(
            Expr::Call(Box::new(Call {
                callee: Expr::Ident(ident("print")),
                span: Default::default(),
                kind: CallKind::Fn(vec![num_lit(10.0), num_lit(5.0)])
            })),
            literal
        );
    }

    #[test]
    fn nested() {
        let tokens = [
            Ident("hugo"),
            Dot,
            Ident("name"),
            Dot,
            Ident("print"),
            ParenO,
            ParenC,
        ]
        .map(token)
        .into();
        let literal = parse_call(tokens);
        assert_eq!(
            Expr::Call(Box::new(Call {
                callee: Expr::Call(Box::new(Call {
                    callee: Expr::Call(Box::new(Call {
                        callee: Expr::Ident(ident("hugo")),
                        span: Default::default(),
                        kind: CallKind::Field(ident("name"))
                    })),
                    span: Default::default(),
                    kind: CallKind::Field(ident("print"))
                })),
                span: Default::default(),
                kind: CallKind::Fn(vec![])
            })),
            literal
        );
    }

    #[test]
    fn with_exprs() {
        // print((10 + 5).abs())
        let tokens = [
            Ident("print"),
            ParenO,
            ParenO,
            Number(10.0),
            Plus,
            Number(5.0),
            ParenC,
            Dot,
            Ident("abs"),
            ParenO,
            ParenC,
            ParenC,
        ]
        .map(token)
        .into();
        let literal = parse_call(tokens);
        assert_eq!(
            Expr::Call(Box::new(Call {
                callee: Expr::Ident(ident("print")),
                span: Default::default(),
                kind: CallKind::Fn(vec![Expr::Call(Box::new(Call {
                    callee: Expr::Call(Box::new(Call {
                        callee: Expr::BinaryOp(Box::new(BinaryOp {
                            span: Default::default(),
                            lhs: num_lit(10.0),
                            rhs: num_lit(5.0),
                            kind: BinaryOpKind::Add
                        })),
                        span: Default::default(),
                        kind: CallKind::Field(ident("abs"))
                    })),
                    span: Default::default(),
                    kind: CallKind::Fn(vec![])
                })),])
            })),
            literal
        );
    }
}

mod primary {
    use super::prelude::*;

    fn parse_primary(tokens: Vec<Token>) -> Expr {
        let mut parser = parser(tokens);
        parser.primary().unwrap()
    }

    #[test]
    fn ident_test() {
        let tokens = [Ident("tokens")].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(Expr::Ident(ident("tokens")), literal);
    }

    #[test]
    fn string() {
        let tokens = [Number(10.0)].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(num_lit(10.0), literal);
    }

    #[test]
    fn number() {
        let tokens = [String("uwu".to_string())].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(
            Expr::Literal(Literal::String("uwu".to_string(), Span::dummy())),
            literal
        );
    }

    #[test]
    fn empty_object() {
        let tokens = [BraceO, BraceC].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(Expr::Literal(Literal::Object(Span::dummy())), literal);
    }

    #[test]
    fn empty_array() {
        let tokens = [BracketO, BracketC].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(
            Expr::Literal(Literal::Array(Vec::new(), Span::dummy())),
            literal
        );
    }

    #[test]
    fn r#false() {
        let tokens = [False].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(
            Expr::Literal(Literal::Boolean(false, Span::dummy())),
            literal
        );
    }

    #[test]
    fn r#true() {
        let tokens = [True].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(
            Expr::Literal(Literal::Boolean(true, Span::dummy())),
            literal
        );
    }

    #[test]
    fn null() {
        let tokens = [Null].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(Expr::Literal(Literal::Null(Span::dummy())), literal);
    }

    #[test]
    fn empty_array_literal() {
        let tokens = [BracketO, BracketC].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(
            Expr::Literal(Literal::Array(Vec::new(), Span::dummy())),
            literal
        );
    }

    #[test]
    fn single_array_literal() {
        let tokens = [BracketO, Number(10.0), BracketC].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(
            Expr::Literal(Literal::Array(vec![num_lit(10.0)], Span::dummy())),
            literal
        );
    }

    #[test]
    fn single_array_literal_trailing_comma() {
        let tokens = [BracketO, Number(10.0), Comma, BracketC].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(
            Expr::Literal(Literal::Array(vec![num_lit(10.0)], Span::dummy())),
            literal
        );
    }

    #[test]
    fn two_array_literal() {
        let tokens = [BracketO, Number(10.0), Comma, Number(10.0), BracketC]
            .map(token)
            .into();
        let literal = parse_primary(tokens);
        assert_eq!(
            Expr::Literal(Literal::Array(
                vec![num_lit(10.0), num_lit(10.0)],
                Span::dummy()
            )),
            literal
        );
    }

    #[test]
    fn two_array_literal_trailing_comma() {
        let tokens = [BracketO, Number(10.0), Comma, Number(10.0), Comma, BracketC]
            .map(token)
            .into();
        let literal = parse_primary(tokens);
        assert_eq!(
            Expr::Literal(Literal::Array(
                vec![num_lit(10.0), num_lit(10.0)],
                Span::dummy()
            )),
            literal
        );
    }

    #[test]
    fn two_array_literal_no_comma() {
        let tokens = [BracketO, Number(10.0), Number(10.0), BracketC]
            .map(token)
            .into();
        let mut parser = parser(tokens);
        let expr = parser.primary();
        assert!(expr.is_err());
    }
}
