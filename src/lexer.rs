use std::{fmt, iter::Peekable, str::Chars};

pub enum LexerError {
    UnknownChar(char, Span),
    InvalidNumber(String, Span),
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownChar(c, span)     => f.write_str(&format!("Unknown character `{c}` at {span:?}")),
            Self::InvalidNumber(str, span) => f.write_str(&format!("Invalid number `{str}` at {span:?} ")),
        } 
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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
            column: 1,
            input
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.input.next()?;
        self.current_index += ch.len_utf8();

        if ch == '\n' {
            self.line += 1;
        }

        self.column += 1;

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
                    _ => Ok(self.create_token(TokenKind::Let, start)),
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
                
                let num = num_str.parse::<f32>().map_err(|_| LexerError::InvalidNumber(num_str, self.create_span(start)))?;
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
            _ => Err(LexerError::UnknownChar(char, self.create_span(start))),
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
