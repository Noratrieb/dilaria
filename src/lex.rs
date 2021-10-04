#![allow(dead_code)]

use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, Ord, Eq, Hash)]
pub struct Span {
    start: usize,
    len: usize,
}

impl Span {
    pub fn new(start: usize, len: usize) -> Self {
        Self { start, len }
    }

    pub fn single(start: usize) -> Self {
        Self { start, len: 1 }
    }
}

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

    fn maybe_next_char(
        &mut self,
        expect_char: char,
        true_type: TokenType,
        false_type: TokenType,
        start: usize,
    ) -> Token {
        if self.expect(expect_char) {
            let _ = self.code.next(); // consume =
            Token {
                span: Span::new(start, 2),
                kind: true_type,
            }
        } else {
            Token {
                span: Span::single(start),
                kind: false_type,
            }
        }
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
                LexState::Equal(start) => {
                    self.maybe_next_char('=', TokenType::EqualEqual, TokenType::Equal, start);
                }
                LexState::Bang(start) => {
                    return if self.expect('=') {
                        let _ = self.code.next(); // consume =;
                        Some(Ok(Token {
                            span: Span::single(start),
                            kind: TokenType::BangEqual,
                        }))
                    } else {
                        Some(Err(LexError))
                    };
                }
                LexState::GreaterThan(start) => {
                    self.maybe_next_char(
                        '=',
                        TokenType::GreaterThanEqual,
                        TokenType::GreaterThan,
                        start,
                    );
                }
                LexState::LessThan(start) => {
                    self.maybe_next_char('=', TokenType::LessThanEqual, TokenType::LessThan, start);
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct LexError;
