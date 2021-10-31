use crate::ast::BinaryOp;
use crate::parse::Parser;
use prelude::*;

mod prelude {
    pub(super) use super::{parser, test_literal_bin_op, test_number_literal, token};
    pub(super) use crate::ast::{BinaryOp, BinaryOpKind, Expr, Literal};
    pub(super) use crate::errors::Span;
    pub(super) use crate::lex::{Token, TokenType};
}

fn token(kind: TokenType) -> Token {
    Token {
        span: Span::dummy(),
        kind,
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
            lhs: Expr::Literal(Literal::Number(10.0, Span::dummy())),
            rhs: Expr::Literal(Literal::Number(4.0, Span::dummy())),
            kind: expected_op_kind
        })),
        factor
    );
}

fn test_number_literal<F: FnOnce(Vec<Token<'_>>) -> Expr>(parser: F) {
    let tokens = [TokenType::Number(10.0)].map(token).into();
    let unary = parser(tokens);
    assert_eq!(Expr::Literal(Literal::Number(10.0, Span::dummy())), unary);
}

mod expr {
    use super::prelude::*;
    use crate::ast::{UnaryOp, UnaryOpKind};
    use TokenType::*;

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
                lhs: Expr::Literal(Literal::Number(10.0, Span::dummy())),
                rhs: Expr::BinaryOp(Box::new(BinaryOp {
                    span: Span::dummy(),
                    lhs: Expr::Literal(Literal::Number(20.0, Span::dummy())),
                    rhs: Expr::Literal(Literal::Number(100.0, Span::dummy())),

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
                lhs: Expr::Literal(Literal::Number(10.0, Span::dummy())),
                rhs: Expr::UnaryOp(Box::new(UnaryOp {
                    span: Span::dummy(),
                    expr: Expr::Literal(Literal::Number(10.0, Span::dummy())),
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
                lhs: Expr::Literal(Literal::Number(10.0, Span::dummy())),
                rhs: Expr::BinaryOp(Box::new(BinaryOp {
                    span: Span::dummy(),
                    lhs: Expr::Literal(Literal::Number(20.0, Span::dummy())),
                    rhs: Expr::Literal(Literal::Number(30.0, Span::dummy())),

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
        test_literal_bin_op(TokenType::Or, BinaryOpKind::Or, parse_logical_or);
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
        test_literal_bin_op(TokenType::And, BinaryOpKind::And, parse_logical_and);
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
        test_literal_bin_op(TokenType::BangEqual, BinaryOpKind::NotEqual, parse_equality);
    }

    #[test]
    fn equal() {
        test_literal_bin_op(TokenType::EqualEqual, BinaryOpKind::Equal, parse_equality);
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
        test_literal_bin_op(TokenType::Greater, BinaryOpKind::Greater, parse_comparison);
    }

    #[test]
    fn greater_equal() {
        test_literal_bin_op(
            TokenType::GreaterEqual,
            BinaryOpKind::GreaterEqual,
            parse_comparison,
        );
    }

    #[test]
    fn less() {
        test_literal_bin_op(TokenType::Less, BinaryOpKind::Less, parse_comparison);
    }

    #[test]
    fn less_equal() {
        test_literal_bin_op(
            TokenType::LessEqual,
            BinaryOpKind::LessEqual,
            parse_comparison,
        );
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
        test_literal_bin_op(TokenType::Plus, BinaryOpKind::Add, parse_term);
    }

    #[test]
    fn sub() {
        test_literal_bin_op(TokenType::Minus, BinaryOpKind::Sub, parse_term);
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
        test_literal_bin_op(TokenType::Asterisk, BinaryOpKind::Mul, parse_factor);
    }

    #[test]
    fn divide() {
        test_literal_bin_op(TokenType::Slash, BinaryOpKind::Div, parse_factor);
    }

    #[test]
    fn modulo() {
        test_literal_bin_op(TokenType::Percent, BinaryOpKind::Mod, parse_factor);
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
        let tokens = [TokenType::Not, TokenType::True].map(token).into();
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
        let tokens = [TokenType::Minus, TokenType::Number(10.0)]
            .map(token)
            .into();
        let unary = parse_unary(tokens);
        assert_eq!(
            Expr::UnaryOp(Box::new(UnaryOp {
                span: Span::dummy(),
                expr: Expr::Literal(Literal::Number(10.0, Span::dummy())),
                kind: UnaryOpKind::Neg
            })),
            unary
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
    fn ident() {
        let tokens = [TokenType::Ident("tokens")].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(Expr::Ident("tokens".to_string(), Span::dummy()), literal);
    }

    #[test]
    fn string() {
        let tokens = [TokenType::Number(10.0)].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(Expr::Literal(Literal::Number(10.0, Span::dummy())), literal);
    }

    #[test]
    fn number() {
        let tokens = [TokenType::String("uwu".to_string())].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(
            Expr::Literal(Literal::String("uwu".to_string(), Span::dummy())),
            literal
        );
    }

    #[test]
    fn empty_object() {
        let tokens = [TokenType::BraceO, TokenType::BraceC].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(Expr::Literal(Literal::Object(Span::dummy())), literal);
    }

    #[test]
    fn empty_array() {
        let tokens = [TokenType::BracketO, TokenType::BracketC].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(
            Expr::Literal(Literal::Array(Vec::new(), Span::dummy())),
            literal
        );
    }

    #[test]
    fn r#false() {
        let tokens = [TokenType::False].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(
            Expr::Literal(Literal::Boolean(false, Span::dummy())),
            literal
        );
    }

    #[test]
    fn r#true() {
        let tokens = [TokenType::True].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(
            Expr::Literal(Literal::Boolean(true, Span::dummy())),
            literal
        );
    }

    #[test]
    fn null() {
        let tokens = [TokenType::Null].map(token).into();
        let literal = parse_primary(tokens);
        assert_eq!(Expr::Literal(Literal::Null(Span::dummy())), literal);
    }
}
