#![allow(dead_code)]

use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, Ord, Eq, Hash)]
struct Span(usize);

#[derive(Debug, Clone)]
pub struct Token<'code> {
    span: Span,
    kind: TokenType<'code>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType<'code> {
    // keywords
    Let,
    Fn,
    If,
    Else,
    Loop,
    While,
    For,
    True,
    False,
    Null,
    And,
    Or,
    Not,
    // literals
    String(&'code str),
    Number(f64),
    // ident
    Ident(&'code str),
    // punctuation
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Asterisk,
    /// /
    Slash,
    /// %
    Percent,
    /// {
    BraceO,
    /// }
    BraceC,
    /// [
    BracketO,
    /// ]
    BracketC,
    /// (
    ParenO,
    /// )
    ParenC,
    /// .
    Dot,
    /// ,
    Comma,
    // =
    Equal,
    /// ==
    EqualEqual,
    /// !=
    BangEqual,
    /// >
    GreaterThan,
    /// <
    LessThan,
    /// >=
    GreaterThanEqual,
    /// <=
    LessThanEqual,
}

#[derive(Debug, Clone)]
pub struct Lexer<'code> {
    code: Peekable<CharIndices<'code>>,
    state: LexState,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum LexState {
    Init,
    StrLit(usize),
    NumLit(usize),
    Ident(usize),
    Equal(usize),
    Bang(usize),
    GreaterThan(usize),
    LessThan(usize),
}

impl<'code> Lexer<'code> {
    pub fn lex(code: &'code str) -> Self {
        Self {
            code: code.char_indices().peekable(),
            state: LexState::Init,
        }
    }

    fn expect(&mut self, expected: char) -> bool {
        self.code
            .peek()
            .map(|(_, char)| *char == expected)
            .unwrap_or(false)
    }
}

impl<'code> Iterator for Lexer<'code> {
    type Item = Result<Token<'code>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.state {
                LexState::Init => match self.code.next() {
                    _ => {}
                },
                LexState::StrLit(_) => {}
                LexState::NumLit(_) => {}
                LexState::Ident(_) => {}
                LexState::Equal(_) => {}
                LexState::Bang(start) => {
                    return if self.expect('=') {
                        let _ = self.code.next();
                        Some(Ok(Token {
                            span: Span(start),
                            kind: TokenType::BangEqual,
                        }))
                    } else {
                        Some(Err(LexError))
                    }
                }
                LexState::GreaterThan(_) => {}
                LexState::LessThan(_) => {}
            }
        }
    }
}

#[derive(Debug)]
pub struct LexError;
