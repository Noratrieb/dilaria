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
    pub(crate) kind: TokenType<'code>,
}

impl<'code> Token<'code> {
    fn single_span(start: usize, kind: TokenType<'code>) -> Token<'code> {
        Self {
            span: Span::single(start),
            kind,
        }
    }

    fn new(span: Span, kind: TokenType<'code>) -> Token<'code> {
        Self { span, kind }
    }
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
    String(String),
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
    src: &'code str,
}

impl<'code> Lexer<'code> {
    pub fn lex(code: &'code str) -> Self {
        Self {
            code: code.char_indices().peekable(),
            src: code,
        }
    }

    fn expect(&mut self, expected: char) -> bool {
        self.code
            .peek()
            .map(|(_, char)| *char == expected)
            .unwrap_or(false)
    }

    fn maybe_next_char<'a>(
        &mut self,
        expect_char: char,
        true_type: TokenType<'a>,
        false_type: TokenType<'a>,
        start: usize,
    ) -> Token<'a> {
        if self.expect(expect_char) {
            let _ = self.code.next(); // consume first one
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
                '+' => break Token::single_span(start, TokenType::Plus),
                '-' => break Token::single_span(start, TokenType::Minus),
                '*' => break Token::single_span(start, TokenType::Asterisk),
                '/' => break Token::single_span(start, TokenType::Slash),
                '%' => break Token::single_span(start, TokenType::Percent),
                '{' => break Token::single_span(start, TokenType::BraceO),
                '}' => break Token::single_span(start, TokenType::BraceC),
                '[' => break Token::single_span(start, TokenType::BracketO),
                ']' => break Token::single_span(start, TokenType::BracketC),
                '(' => break Token::single_span(start, TokenType::ParenO),
                ')' => break Token::single_span(start, TokenType::ParenC),
                '.' => break Token::single_span(start, TokenType::Dot),
                ',' => break Token::single_span(start, TokenType::Comma),
                '=' => {
                    break self.maybe_next_char(
                        '=',
                        TokenType::EqualEqual,
                        TokenType::Equal,
                        start,
                    );
                }
                '!' => {
                    if self.expect('=') {
                        let _ = self.code.next(); // consume =;
                        break Token {
                            span: Span::single(start),
                            kind: TokenType::BangEqual,
                        };
                    } else {
                        return Some(Err(LexError("Expected '=' after '!'".to_string())));
                    };
                }
                '>' => {
                    break self.maybe_next_char(
                        '=',
                        TokenType::GreaterThanEqual,
                        TokenType::GreaterThan,
                        start,
                    );
                }
                '<' => {
                    break self.maybe_next_char(
                        '=',
                        TokenType::LessThanEqual,
                        TokenType::LessThan,
                        start,
                    );
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
                                return Some(Err(LexError(
                                    "reached EOF expecting '\"'".to_string(),
                                )))
                            }
                        }
                    };
                    break Token::new(Span::new(start, end - start), TokenType::String(buffer));
                }
                char => {
                    if char.is_ascii_digit() {
                        let mut had_dot = false;
                        let end = loop {
                            // peek here because the character signaling the end should not be consumed
                            match self.code.peek() {
                                Some((_, '.')) if !had_dot => {
                                    let _ = self.code.next();
                                    had_dot = true;
                                }
                                Some((_, next_char)) if next_char.is_ascii_digit() => {
                                    let _ = self.code.next();
                                }
                                Some((end, _)) => break *end,
                                None => break self.src.len(), // reached EOF, so parse this number
                            }
                        };
                        let number_str = &self.src[start..end];
                        let number = number_str
                            .parse::<f64>()
                            .map_err(|err| LexError(err.to_string()));
                        match number {
                            Ok(number) => {
                                break Token::new(
                                    Span::new(start, end - start),
                                    TokenType::Number(number),
                                )
                            }
                            Err(err) => return Some(Err(err)),
                        }
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
                            Span::new(start, end),
                            keyword_or_ident(&self.src[start..end]),
                        );
                    } else {
                        return Some(Err(LexError(format!("Invalid character: {}", char))));
                    }
                }
            }
        };

        Some(Ok(token))
    }
}

fn keyword_or_ident(name: &str) -> TokenType {
    // make this efficient using the trie pattern
    // ignore that unicode exists, because all keywords are in ascii
    // we need to use bytes though instead of indexing into the string directly to avoid panics
    let bs = name.as_bytes();
    let len = bs.len();
    // there are no single letter keywords
    if len < 2 {
        return TokenType::Ident(name);
    }
    match bs[0] {
        // loop && let
        b'l' => match bs[1] {
            b'o' if len == 4 && bs[2..4] == *b"op" => TokenType::Loop,
            b'e' if len == 3 && bs[2] == b't' => TokenType::Let,
            _ => TokenType::Ident(name),
        },
        // for && fn && false
        b'f' => match bs[1] {
            b'n' if len == 2 => TokenType::Fn,
            b'o' if len == 3 && bs[2] == b'r' => TokenType::For,
            b'a' if len == 5 && bs[2..5] == *b"lse" => TokenType::False,
            _ => TokenType::Ident(name),
        },
        // if
        b'i' if len == 2 && bs[1] == b'f' => TokenType::If,
        // else
        b'e' if len == 4 && bs[1..4] == *b"lse" => TokenType::Else,
        // while
        b'w' if len == 5 && bs[1..5] == *b"hile" => TokenType::While,
        // true
        b't' if len == 4 && bs[1..4] == *b"rue" => TokenType::True,
        // null && not
        b'n' => match bs[1] {
            b'u' if len == 4 && bs[2..4] == *b"ll" => TokenType::Null,
            b'o' if len == 3 && bs[2] == b't' => TokenType::Not,
            _ => TokenType::Ident(name),
        },
        // and
        b'a' if len == 3 && bs[1..3] == *b"nd" => TokenType::And,
        // or
        b'o' if len == 2 && bs[1] == b'r' => TokenType::Or,
        _ => TokenType::Ident(name),
    }
}

fn is_valid_ident_part(char: char) -> bool {
    char.is_alphanumeric() || char == '_'
}

fn is_valid_ident_start(char: char) -> bool {
    char.is_alphabetic() || char == '_'
}

#[derive(Debug)]
pub struct LexError(String);

#[cfg(test)]
mod test {
    use crate::lex::Lexer;
    use crate::lex::TokenType::{self, *};

    type StdString = std::string::String;

    fn lex_types(str: &str) -> Vec<TokenType> {
        let lexer = Lexer::lex(str);
        lexer.map(|token| token.unwrap().kind).collect::<Vec<_>>()
    }

    fn lex_test(code: &str, expected: Vec<TokenType>) {
        assert_eq!(lex_types(code), expected)
    }

    #[test]
    fn smiley_face() {
        lex_test(
            ">>.<<",
            vec![GreaterThan, GreaterThan, Dot, LessThan, LessThan],
        )
    }

    #[test]
    fn greater_than_less_than_equal() {
        lex_test(
            ">= <= == < < >=",
            vec![
                GreaterThanEqual,
                LessThanEqual,
                EqualEqual,
                LessThan,
                LessThan,
                GreaterThanEqual,
            ],
        )
    }

    #[test]
    fn no_no_no() {
        lex_test("!= != = !=", vec![BangEqual, BangEqual, Equal, BangEqual])
    }

    #[test]
    fn braces_brackets_parens() {
        lex_test(
            "{([]]}",
            vec![BraceO, ParenO, BracketO, BracketC, BracketC, BraceC],
        );
    }

    #[test]
    fn braces_brackets_parens_whitespace() {
        lex_test(
            "{     (   [      ]         ]   
            
            }",
            vec![BraceO, ParenO, BracketO, BracketC, BracketC, BraceC],
        );
    }

    #[test]
    fn fancy_stuff() {
        lex_test(
            ". ,- * -, .",
            vec![Dot, Comma, Minus, Asterisk, Minus, Comma, Dot],
        )
    }

    #[test]
    fn comments() {
        lex_test("fn # fn", vec![Fn]);
    }

    #[test]
    fn long_multiline_comment() {
        lex_test(
            "fn ## hello i am something
         
         i span multiple lines

        will you love me? 🥺🥺🥺🥺🥺

    pls :) o(*￣▽￣*)ブ
     
       i like the indentation here ngl |     sneak for -> ## for ## <- sneak for
         ## and",
            vec![Fn, For, And],
        )
    }

    #[test]
    fn terminate_multiline_comment_correctly() {
        lex_test(
            "fn ## # no not here :( ## let # ## <- this is commented out 
            # so no multiline comment
            ## 
            
            here it starts
            # let #
            # # and
            ## or
            ",
            vec![Fn, Let, Or],
        )
    }

    #[test]
    fn greeting() {
        lex_test("-.- /%", vec![Minus, Dot, Minus, Slash, Percent])
    }

    #[test]
    fn countdown() {
        lex_test(
            "3 . . 2 . . 1 . . 0",
            vec![
                Number(3.0),
                Dot,
                Dot,
                Number(2.0),
                Dot,
                Dot,
                Number(1.0),
                Dot,
                Dot,
                Number(0.0),
            ],
        )
    }

    #[test]
    fn larger_numbers() {
        lex_test(
            "123456789, 123456789.1234, 64785903",
            vec![
                Number(123456789.0),
                Comma,
                Number(123456789.1234),
                Comma,
                Number(64785903.0),
            ],
        )
    }

    #[test]
    fn string() {
        lex_test(r#""uwu""#, vec![String("uwu".to_string())])
    }

    #[test]
    fn strings() {
        lex_test(
            r#"(  "hi" "uwu" "\"uwu\""  "no \\ u" )"#,
            vec![
                ParenO,
                String("hi".to_string()),
                String("uwu".to_string()),
                String("\"uwu\"".to_string()),
                String("no \\ u".to_string()),
                ParenC,
            ],
        )
    }

    #[test]
    fn keywords() {
        lex_test(
            "let fn if else loop while for true false null and not or",
            vec![
                Let, Fn, If, Else, Loop, While, For, True, False, Null, And, Not, Or,
            ],
        )
    }

    #[test]
    fn keyword_and_ident() {
        lex_test(
            "let variable be a loop if false is true",
            vec![
                Let,
                Ident("variable"),
                Ident("be"),
                Ident("a"),
                Loop,
                If,
                False,
                Ident("is"),
                True,
            ],
        )
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
        ];
        let sentences = words
            .iter()
            .map(|word| format!("{} ", word))
            .collect::<StdString>();
        let expected = words.map(TokenType::Ident).to_vec();

        lex_test(&sentences, expected)
    }

    #[test]
    fn serious_program() {
        lex_test(
            r#"let string = "hallol"
        let number = 5
        let me out ._.
        fn world() {
            if number == 5 or true == false and not false {
                print("Hello \\ World!")
            }
        }"#,
            vec![
                Let,
                Ident("string"),
                Equal,
                String("hallol".to_string()),
                Let,
                Ident("number"),
                Equal,
                Number(5.0),
                Let,
                Ident("me"),
                Ident("out"),
                Dot,
                Ident("_"),
                Dot,
                Fn,
                Ident("world"),
                ParenO,
                ParenC,
                BraceO,
                If,
                Ident("number"),
                EqualEqual,
                Number(5.0),
                Or,
                True,
                EqualEqual,
                False,
                And,
                Not,
                False,
                BraceO,
                Ident("print"),
                ParenO,
                String("Hello \\ World!".to_string()),
                ParenC,
                BraceC,
                BraceC,
            ],
        )
    }
}
