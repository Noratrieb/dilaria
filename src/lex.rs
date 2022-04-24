//!
//! The lex module lexes the source code into Tokens
//!
//! For error handling, there is a single `Error` token, which contains the error. The lexer
//! is an iterator, and can therefore be used without any allocations

use std::{iter::Peekable, str::CharIndices};

use crate::{
    errors::{CompilerError, Span},
    gc::Symbol,
    RtAlloc,
};

///
/// A single token generated from the lexer
///
/// For example `for`, `"hello"`, `main` or `.`
#[derive(Debug, Clone)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    fn single_span(start: usize, kind: TokenKind) -> Token {
        Self {
            span: Span::single(start),
            kind,
        }
    }

    fn new(span: Span, kind: TokenKind) -> Token {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // keywords
    Let,
    Print,
    Fn,
    If,
    Else,
    Loop,
    While,
    For,
    Break,
    Return,
    True,
    False,
    Null,
    And,
    Or,
    Not,
    // literals
    String(Symbol),
    Number(f64),
    // ident
    Ident(Symbol),
    // punctuation
    /// ;
    Semi,
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
    Greater,
    /// <
    Less,
    /// >=
    GreaterEqual,
    /// <=
    LessEqual,

    /// An error occurred. It's boxed to save space, since `CompilerError` is > 6 `usize` big
    Error(Box<CompilerError>),
}

#[derive(Debug)]
pub struct Lexer<'code, 'gc> {
    code: Peekable<CharIndices<'code>>,
    src: &'code str,
    rt_alloc: &'gc mut RtAlloc,
}

impl<'code, 'gc> Lexer<'code, 'gc> {
    pub fn new(code: &'code str, rt_alloc: &'gc mut RtAlloc) -> Self {
        Self {
            code: code.char_indices().peekable(),
            src: code,
            rt_alloc,
        }
    }

    fn expect(&mut self, expected: char) -> bool {
        self.code
            .peek()
            .map_or(false, |(_, char)| *char == expected)
    }

    fn maybe_next_char(
        &mut self,
        expect_char: char,
        true_type: TokenKind,
        false_type: TokenKind,
        start: usize,
    ) -> Token {
        if self.expect(expect_char) {
            let _ = self.code.next(); // consume first one
            Token {
                span: Span::start_len(start, start + 2),
                kind: true_type,
            }
        } else {
            Token {
                span: Span::single(start),
                kind: false_type,
            }
        }
    }

    fn keyword_or_ident(&mut self, name: &str) -> TokenKind {
        match name {
            "loop" => TokenKind::Loop,
            "let" => TokenKind::Let,
            "fn" => TokenKind::Fn,
            "for" => TokenKind::For,
            "false" => TokenKind::False,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "break" => TokenKind::Break,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "null" => TokenKind::Null,
            "not" => TokenKind::Not,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "print" => TokenKind::Print,
            _ => TokenKind::Ident(self.rt_alloc.intern_string(name)),
        }
    }
}

impl<'code, 'gc> Iterator for Lexer<'code, 'gc> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = loop {
            let (start, char) = self.code.next()?;
            match char {
                _ if char.is_whitespace() => {}
                '#' => {
                    // only peek so we don't skip the \n if the # is at the end
                    if let Some((_, '#')) = self.code.peek() {
                        let _ = self.code.next();
                        loop {
                            if let Some((_, '#')) | None = self.code.next() {
                                if let Some((_, '#')) | None = self.code.next() {
                                    break;
                                }
                            }
                        }
                    } else {
                        loop {
                            if let Some((_, '\n')) | None = self.code.next() {
                                break;
                            }
                        }
                    }
                }
                ';' => break Token::single_span(start, TokenKind::Semi),
                '+' => break Token::single_span(start, TokenKind::Plus),
                '-' => break Token::single_span(start, TokenKind::Minus),
                '*' => break Token::single_span(start, TokenKind::Asterisk),
                '/' => break Token::single_span(start, TokenKind::Slash),
                '%' => break Token::single_span(start, TokenKind::Percent),
                '{' => break Token::single_span(start, TokenKind::BraceO),
                '}' => break Token::single_span(start, TokenKind::BraceC),
                '[' => break Token::single_span(start, TokenKind::BracketO),
                ']' => break Token::single_span(start, TokenKind::BracketC),
                '(' => break Token::single_span(start, TokenKind::ParenO),
                ')' => break Token::single_span(start, TokenKind::ParenC),
                '.' => break Token::single_span(start, TokenKind::Dot),
                ',' => break Token::single_span(start, TokenKind::Comma),
                '=' => {
                    break self.maybe_next_char(
                        '=',
                        TokenKind::EqualEqual,
                        TokenKind::Equal,
                        start,
                    );
                }
                '!' => {
                    break if self.expect('=') {
                        let _ = self.code.next(); // consume =;
                        Token::new(Span::start_len(start, start + 2), TokenKind::BangEqual)
                    } else {
                        Token::new(
                            Span::single(start),
                            TokenKind::Error(Box::new(CompilerError::with_note(
                                Span::single(start),
                                "Expected '=' after '!'".to_string(),
                                "If you meant to use it for negation, use `not`".to_string(),
                            ))),
                        )
                    };
                }
                '>' => {
                    break self.maybe_next_char(
                        '=',
                        TokenKind::GreaterEqual,
                        TokenKind::Greater,
                        start,
                    );
                }
                '<' => {
                    break self.maybe_next_char('=', TokenKind::LessEqual, TokenKind::Less, start);
                }
                '"' => {
                    let mut buffer = String::new();
                    let mut escaped = false;
                    let end = loop {
                        match self.code.next() {
                            Some((end, '"')) if !escaped => break end,
                            Some((_, '\\')) if !escaped => escaped = true,
                            Some((_, char)) => {
                                escaped = false;
                                buffer.push(char);
                            }
                            None => {
                                return Some(Token::new(
                                    Span::single(start),
                                    TokenKind::Error(Box::new(CompilerError::with_note(
                                        Span::single(start), // no not show the whole literal, this does not make sense
                                        "String literal not closed".to_string(),
                                        "Close the literal using '\"'".to_string(),
                                    ))),
                                ));
                            }
                        }
                    };
                    break Token::new(
                        Span::start_end(start, end),
                        TokenKind::String(self.rt_alloc.intern_string(&buffer)),
                    );
                }
                char => {
                    if char.is_ascii_digit() {
                        let mut num_buffer = String::from(char); // we need to ignore `_`
                        let mut had_dot = false;
                        let end = loop {
                            // peek here because the character signaling the end should not be consumed
                            match self.code.peek().copied() {
                                Some((_, '.')) if !had_dot => {
                                    let _ = self.code.next();
                                    num_buffer.push('.');
                                    had_dot = true;
                                }
                                Some((_, '_')) => {
                                    let _ = self.code.next();
                                }
                                Some((_, next_char)) if next_char.is_ascii_digit() => {
                                    let _ = self.code.next();
                                    num_buffer.push(next_char);
                                }
                                Some((end, _)) => break end,
                                None => break self.src.len(), // reached EOF, so parse this number
                            }
                        };
                        let number_str = &num_buffer;
                        let span = Span::start_end(start, end);
                        let number = number_str.parse::<f64>();
                        break match number {
                            Ok(number) if number.is_infinite() => {
                                Token::new(span, TokenKind::Error(Box::new(CompilerError::with_note(
                                    span,
                                    "Number literal too long".to_string(),
                                    "A number literal cannot be larger than a 64 bit float can represent"
                                        .to_string(),
                                ))))
                            }
                            Ok(number) => Token::new(span, TokenKind::Number(number)),
                            Err(err) => Token::new(span, TokenKind::Error(Box::new(CompilerError::with_note(
                                span,
                                "Invalid number".to_string(),
                                err.to_string(),
                            )))),
                        };
                    } else if is_valid_ident_start(char) {
                        // it must be an identifier
                        let end = loop {
                            match self.code.peek() {
                                Some((_, char)) if is_valid_ident_part(*char) => {
                                    let _ = self.code.next(); // consume identifier part
                                }
                                Some((end, _)) => break *end,
                                None => break self.src.len(),
                            }
                        };
                        break Token::new(
                            Span::start_end(start, end),
                            self.keyword_or_ident(&self.src[start..end]),
                        );
                    } else {
                        break Token::new(
                            Span::single(start),
                            TokenKind::Error(Box::new(CompilerError::with_note(
                                Span::single(start),
                                format!("Unexpected character: '{}'", char),
                                "Character is not allowed outside of string literals and comments"
                                    .to_string(),
                            ))),
                        );
                    }
                }
            }
        };

        Some(token)
    }
}

fn is_valid_ident_part(char: char) -> bool {
    char.is_alphanumeric() || char == '_'
}

fn is_valid_ident_start(char: char) -> bool {
    char.is_alphabetic() || char == '_'
}

#[cfg(test)]
mod test {
    use crate::{lex::Lexer, RtAlloc};

    type StdString = std::string::String;

    macro_rules! lex_test {
        ($code:expr) => {
            // SAFETY: we only work in this tiny scope
            let mut runtime = unsafe { RtAlloc::new() };

            let lexer = Lexer::new($code, &mut runtime);
            let tokens = lexer.map(|token| token.kind).collect::<Vec<_>>();

            insta::assert_debug_snapshot!(tokens);
        };
    }

    #[test]
    fn smiley_face() {
        lex_test!(">>.<<");
    }

    #[test]
    fn greater_than_less_than_equal() {
        lex_test!(">= <= == < < >=");
    }

    #[test]
    fn no_no_no() {
        lex_test!("!= != = !=");
    }

    #[test]
    fn braces_brackets_parens() {
        lex_test!("{([]]}");
    }

    #[test]
    fn braces_brackets_parens_whitespace() {
        lex_test!(
            "{     (   [      ]         ]   
        
        }"
        );
    }

    #[test]
    fn fancy_stuff() {
        lex_test!(". ,- * -, .");
    }

    #[test]
    fn comments() {
        lex_test!("fn # fn");
    }

    #[test]
    fn long_multiline_comment() {
        lex_test!(
            "fn ## hello i am something
     
     i span multiple lines

    will you love me? 🥺🥺🥺🥺🥺

pls :) o(*￣▽￣*)ブ
 
   i like the indentation here ngl |     sneak for -> ## for ## <- sneak for
     ## and"
        );
    }

    #[test]
    fn terminate_multiline_comment_correctly() {
        lex_test!(
            "fn ## # no not here :( ## let # ## <- this is commented out 
        # so no multiline comment
        ## 
        
        here it starts
        # let #
        # # and
        ## or
        "
        );
    }

    #[test]
    fn greeting() {
        lex_test!("-.- /%");
    }

    #[test]
    fn countdown() {
        lex_test!("3 . . 2 . . 1 . . 0");
    }

    #[test]
    fn underscore_number() {
        lex_test!("1_000_000");
    }

    #[test]
    fn trailing_underscore_number() {
        lex_test!("1_00_");
    }

    #[test]
    fn larger_numbers() {
        lex_test!("123456789, 123456789.1234, 64785903");
    }

    #[test]
    fn string() {
        lex_test!(r#""uwu""#);
    }

    #[test]
    fn strings() {
        lex_test!(r#"(  "hi" "uwu" "\"uwu\""  "no \\ u" )"#);
    }

    #[test]
    fn keywords() {
        lex_test!("let fn if else loop while break for true false null and not or print");
    }

    #[test]
    fn keyword_and_ident() {
        lex_test!("let variable be a loop if false is true");
    }

    #[test]
    fn not_quite_a_keyword() {
        let words = [
            "letter",
            "fori",
            "fnfn",
            "iffy",
            "bloop",
            "loopy_yeah",
            "whileTrue",
            "truefalse",
            "falsetrue",
            "nullability",
            "rot",
            "ornot",
            "nor",
            "andnowQuestionMark",
            "notOrAnd",
            "breakMe",
            "Ibreak",
        ];
        let sentences = words
            .iter()
            .map(|word| format!("{} ", word))
            .collect::<StdString>();

        lex_test!(&sentences);
    }

    #[test]
    fn serious_program() {
        lex_test!(
            r#"let string = "hallol"
    let number = 5
    let me out ._.
    fn world() {
        if number == 5 or true == false and not false {
            println("Hello \\ World!")
        }
    }"#
        );
    }
}
