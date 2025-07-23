use std::{iter::Peekable, str::Chars};

pub enum LexerErrorKind {
    UnknownChar(char),
    InvalidNumber(String),
}

impl std::fmt::Display for LexerErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownChar(c)   => f.write_str(&format!("Unknown character: `{c}`")),
            Self::InvalidNumber(s) => f.write_str(&format!("Invalid number: `{s}`"))
        }
    }
}

pub struct LexerError {
    kind: LexerErrorKind,
    span: Span
}

impl LexerError {
    pub fn new(kind: LexerErrorKind, span: Span) -> Self {
        LexerError { kind, span }
    }

    pub fn with_source(&self, source: &str) -> String {
        let mut base = format!("{} at line {}, column {}", self.kind, self.span.line, self.span.column);
        if let Some(line_start) = source.lines().nth(self.span.line-1) {
            let caret_line = " ".repeat(self.span.column-1) + "^";

            base.push_str(&format!(":\n{}", line_start));
            base.push_str(&format!("\n{}", caret_line));
        } 
        base 
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum TokenKind {
    // Keywords 
    Fn,
    Return,
    Let,
    If,
    Else,
    While,

    // Literals
    Identifier(String),
    Number(f32),

    // Operators,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    
    // Symbols
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    Comma,
    Semicolon,

    // Special
    Eof,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fn => f.write_str("fn"),
            Self::Return => f.write_str("return"),
            Self::Let => f.write_str("let"),
            Self::If => f.write_str("if"),
            Self::Else => f.write_str("else"),
            Self::While => f.write_str("while"),
            Self::Identifier(s) => if s.is_empty() {
                f.write_str("identifier")
            } else {
                f.write_str(&format!("identifier: {s}"))
            },
            Self::Number(n) => f.write_str(&format!("number: {n}")),
            Self::Plus => f.write_str("+"),
            Self::Minus => f.write_str("-"),
            Self::Star => f.write_str("*"),
            Self::Slash => f.write_str("/"),
            Self::Percent => f.write_str("%"),
            Self::Equal => f.write_str("="),
            Self::EqualEqual => f.write_str("=="),
            Self::Bang => f.write_str("!"),
            Self::BangEqual => f.write_str("!="),
            Self::Less => f.write_str("<"),
            Self::LessEqual => f.write_str("<="),
            Self::Greater => f.write_str(">"),
            Self::GreaterEqual => f.write_str(">="),
            Self::LeftParen => f.write_str("("),
            Self::RightParen => f.write_str(")"),
            Self::LeftCurly => f.write_str("{"),
            Self::RightCurly => f.write_str("}"),
            Self::Comma => f.write_str(","),
            Self::Semicolon => f.write_str(";"),
            Self::Eof => f.write_str("end of file")
        }
    }
}

#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn new(start: usize, end: usize, line: usize, column: usize) -> Self {
        Span { start, end, line, column }
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }
}

pub struct Lexer<'a> {
    source: &'a str,
    current_index: usize,
    line: usize,
    column: usize,
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let input = source.chars().peekable();
        Lexer {
            source,
            current_index: 0,
            line: 1,
            column: 0,
            input
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.input.next()?;
        self.current_index += ch.len_utf8();

        self.column += 1;
        if ch == '\n' {
            self.line += 1;
            self.column = 0;
        }


        Some(ch)
    }

    fn consume_while<F>(&mut self, condition: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut s = String::new();
        while let Some(&c) = self.input.peek() {
            if condition(c) {
                s.push(c);
                self.next_char();
            } else {
                break;
            }
        }
        s
    }

    pub fn create_span(&self, start: usize) -> Span {
        Span::new(start, self.current_index, self.line, self.column)
    }

    pub fn create_token(&self, kind: TokenKind, start: usize) -> Token {
        Token::new(kind, self.create_span(start))
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        let start = self.current_index;
        self.consume_while(|c| c.is_whitespace());

        let Some(char) = self.next_char() else {
            return Ok(self.create_token(TokenKind::Eof, start))
        };

        match char {
            '(' => Ok(self.create_token(TokenKind::LeftParen,  start)),
            ')' => Ok(self.create_token(TokenKind::RightParen, start)),
            '{' => Ok(self.create_token(TokenKind::LeftCurly,  start)),
            '}' => Ok(self.create_token(TokenKind::RightCurly, start)),
            ',' => Ok(self.create_token(TokenKind::Comma,      start)),
            ';' => Ok(self.create_token(TokenKind::Semicolon,  start)),
            '+' => Ok(self.create_token(TokenKind::Plus,       start)),
            '-' => Ok(self.create_token(TokenKind::Minus,      start)),
            '*' => Ok(self.create_token(TokenKind::Star,       start)),
            '/' => Ok(self.create_token(TokenKind::Slash,      start)),
            '%' => Ok(self.create_token(TokenKind::Percent,    start)),
            '<' => {
                match self.input.peek() {
                    Some('=') => { self.next_char(); Ok(self.create_token(TokenKind::LessEqual, start)) },
                    _ => Ok(self.create_token(TokenKind::Less, start)),
                }
            },
            '>' => {
                match self.input.peek() {
                    Some('=') => { self.next_char(); Ok(self.create_token(TokenKind::GreaterEqual, start)) },
                    _ => Ok(self.create_token(TokenKind::Greater, start)),
                }
            },
            '=' => {
                match self.input.peek() {
                    Some('=') => { self.next_char(); Ok(self.create_token(TokenKind::EqualEqual, start)) },
                    _ => Ok(self.create_token(TokenKind::Equal, start)),
                }
            },
            '!' => {
                match self.input.peek() {
                    Some('=') => { self.next_char(); Ok(self.create_token(TokenKind::BangEqual, start)) },
                    _ => Ok(self.create_token(TokenKind::Bang, start)),
                }
            },
            c if c.is_ascii_digit() => {
                let mut num_str = c.to_string();
                num_str.push_str(&self.consume_while(|c| c.is_ascii_digit()));

                if self.input.peek() == Some(&'.') {
                    self.next_char();
                    num_str.push('.');
                    num_str.push_str(&self.consume_while(|c| c.is_ascii_digit()));
                };
                
                let num = num_str.parse::<f32>().map_err(|_| LexerError::new(LexerErrorKind::InvalidNumber(num_str), self.create_span(start)))?;
                Ok(self.create_token(TokenKind::Number(num), start))
            },
            c if c.is_ascii_alphabetic() || c == '_' => {
                let mut ident = c.to_string();
                ident.push_str(&self.consume_while(|c| c.is_ascii_alphanumeric() || c == '_'));

                match ident.as_str() {
                    "fn"     => Ok(self.create_token(TokenKind::Fn, start)),
                    "return" => Ok(self.create_token(TokenKind::Return, start)),
                    "let"    => Ok(self.create_token(TokenKind::Let, start)),
                    "if"     => Ok(self.create_token(TokenKind::If, start)),
                    "else"   => Ok(self.create_token(TokenKind::Else, start)),
                    "while"  => Ok(self.create_token(TokenKind::While, start)),
                    _        => Ok(self.create_token(TokenKind::Identifier(ident), start))
                }
                
            },
            _ => Err(LexerError::new(LexerErrorKind::UnknownChar(char), self.create_span(start))),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.input.peek().is_none() {
            None
        } else {
            Some(self.next_token()) 
        }
    }
}
