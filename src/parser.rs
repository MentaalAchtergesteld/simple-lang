use std::{iter::Peekable, slice::Iter}; 

use crate::lexer::{Span, Token, TokenKind};

#[derive(Debug)]
pub enum ParserErrorKind {
    ExpectedToken(TokenKind),
    UnexpectedToken(TokenKind),
    UnexpectedEndOfFile,
}

impl std::fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedToken(kind) => f.write_str(&format!("Expected token `{kind}`")),
            Self::UnexpectedToken(kind) => f.write_str(&format!("Unexpected token `{kind}`")),
            Self::UnexpectedEndOfFile => f.write_str(&format!("Unexpected end of file")),
        }
    }
}


#[derive(Debug)]
pub struct ParserError {
    kind: ParserErrorKind,
    span: Span
}

impl ParserError {
    pub fn new(kind: ParserErrorKind, span: Span) -> Self {
        ParserError { kind, span }
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

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f32)
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negate,
    Plus,
    Bang
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Variable(String),
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>
    },
    Unary {
        op: UnaryOp,
        expression: Box<Expression>
    },
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>
    }
}

#[derive(Debug, Clone)]
pub enum ElseBranch {
    Else(Vec<Statement>),
    If(Box<Statement>)
}

#[derive(Debug, Clone)]
pub enum Statement {
    Function {
        name: String,
        params: Vec<String>,
        body: Vec<Statement>
    },
    Declaration {
        name: String,
        value: Expression,
    },
    Assignment {
        name: String,
        value: Expression
    },
    Return(Expression),
    If {
        condition: Expression,
        then_branch: Vec<Statement>,
        else_branch: Option<ElseBranch>,
    },
    While {
        condition: Expression,
        body: Vec<Statement>
    },
    Expression(Expression)
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens: tokens.iter().peekable() }
    }

    fn current_span(&mut self) -> Span {
        self.tokens
            .peek()
            .map(|t| t.span.clone())
            .unwrap_or(Span::new(0, 0, 0, 0))
    }

    fn expect(&mut self, expected: TokenKind) -> Result<&Token, ParserError> {
        match self.tokens.peek() {
            Some(token) if token.kind == expected => Ok(self.tokens.next().unwrap()),
            Some(token) => Err(ParserError::new(
                ParserErrorKind::ExpectedToken(expected),
                token.span.clone()
            )),
            None => Err(ParserError::new(
                ParserErrorKind::UnexpectedEndOfFile,
                self.current_span(),
            ))
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParserError> {
        if let Some(token) = self.tokens.next() {
            if let TokenKind::Identifier(ident) = &token.kind {
                Ok(ident.clone())
            } else {
                Err(ParserError::new(
                    ParserErrorKind::ExpectedToken(TokenKind::Identifier("".into())),
                    token.span.clone()
                ))
            }
        } else {
            Err(ParserError::new(
                ParserErrorKind::UnexpectedEndOfFile,
                self.current_span()
            ))
        }
    }

    fn parse_function_call(&mut self, callee: Expression) -> Result<Expression, ParserError> {
        self.expect(TokenKind::LeftParen)?;
        let mut args = Vec::new();

        loop {
            if let Some(Token { kind: TokenKind::RightParen, .. }) = self.tokens.peek() { break; }
            args.push(self.parse_expression()?);

            match self.tokens.peek() {
                Some(token) => match token.kind {
                    TokenKind::Comma => self.tokens.next(),
                    TokenKind::RightParen => break,
                    _ => return Err(ParserError::new(
                        ParserErrorKind::UnexpectedToken(token.kind.clone()),
                        token.span.clone()
                    ))
                },
                None => return Err(ParserError::new(
                    ParserErrorKind::UnexpectedEndOfFile,
                    self.current_span()
                ))
            };
        }

        self.expect(TokenKind::RightParen)?;
        Ok(Expression::Call { callee: Box::new(callee), args })
    }

    fn parse_primary(&mut self) -> Result<Expression, ParserError> {
        let Some(token) = self.tokens.next() else {
            return Err(ParserError::new(
                ParserErrorKind::UnexpectedEndOfFile,
                self.current_span()
            ))
        };

        let mut expression = match &token.kind {
            TokenKind::Number(n) => Expression::Literal(Literal::Number(*n)),
            TokenKind::Identifier(ident) => Expression::Variable(ident.clone()),
            TokenKind::LeftParen => {
                let expr = self.parse_expression()?;
                self.expect(TokenKind::RightParen)?;
                expr
            },
            kind => return Err(ParserError::new(
                ParserErrorKind::UnexpectedToken(kind.clone()),
                token.span.clone()
            ))
        };
        
        while let Some(Token { kind: TokenKind::LeftParen, .. }) = self.tokens.peek() {
            expression = self.parse_function_call(expression)?;
        }

        Ok(expression)
    }

    fn parse_unary(&mut self) -> Result<Expression, ParserError> {
        if let Some(token @ Token { kind: TokenKind::Minus | TokenKind::Plus | TokenKind::Bang, ..}) = self.tokens.peek() {
            let op = match &token.kind {
                TokenKind::Minus => UnaryOp::Negate,
                TokenKind::Plus => UnaryOp::Plus,
                TokenKind::Bang => UnaryOp::Bang,
                _ => unreachable!()
            };

            self.tokens.next();

            let expression = Box::new(self.parse_unary()?);
            Ok(Expression::Unary { op, expression })
        } else {
            self.parse_primary()
        }
    }

    fn parse_factor(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.parse_unary()?;

        while let Some(token @ Token { kind: TokenKind::Star | TokenKind::Slash | TokenKind::Percent, ..}) = self.tokens.peek() {
            let op = match &token.kind {
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::Slash => BinaryOp::Div,
                TokenKind::Percent => BinaryOp::Mod,
                _ => unreachable!()
            };

            self.tokens.next();
            let right = Box::new(self.parse_unary()?);

            left = Expression::Binary { left: Box::new(left), op, right };
        }
        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.parse_factor()?;

        while let Some(token @ Token { kind: TokenKind::Plus | TokenKind::Minus, .. }) = self.tokens.peek() {
            let op = match &token.kind {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Sub,
                _ => unreachable!()
            };

            self.tokens.next();
            let right = Box::new(self.parse_factor()?);

            left = Expression::Binary { left: Box::new(left), op, right };
        }
        Ok(left)
    } 

    fn parse_comparison(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.parse_term()?;

        while let Some(token @ Token {
            kind: TokenKind::Less | TokenKind::LessEqual | TokenKind::GreaterEqual | TokenKind::Greater, ..
        }) = self.tokens.peek() {
            let op = match &token.kind {
                TokenKind::Less => BinaryOp::Less,
                TokenKind::LessEqual => BinaryOp::LessEqual,
                TokenKind::GreaterEqual => BinaryOp::GreaterEqual,
                TokenKind::Greater => BinaryOp::Greater,
                _ => unreachable!()
            };

            self.tokens.next();
            let right = Box::new(self.parse_term()?);
            
            left = Expression::Binary { left: Box::new(left), op, right };
        }
        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.parse_comparison()?;
        
        while let Some(token @ Token { kind: TokenKind::EqualEqual | TokenKind::BangEqual, ..}) = self.tokens.peek() {
            let op = match &token.kind {
                TokenKind::EqualEqual => BinaryOp::Equal,
                TokenKind::BangEqual => BinaryOp::NotEqual,
                _ => unreachable!(),
            };

            self.tokens.next();
            let right = Box::new(self.parse_comparison()?);

            left = Expression::Binary { left: Box::new(left), op, right };
        }
        Ok(left)
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_equality()
    }

    fn parse_body(&mut self) -> Result<Vec<Statement>, ParserError> {
        self.expect(TokenKind::LeftCurly)?;
        let mut statements = Vec::new();

        while let Some(token) = self.tokens.peek() {
            if token.kind == TokenKind::RightCurly { break }
            statements.push(self.parse_statement()?);
        }

        self.expect(TokenKind::RightCurly)?;
        Ok(statements)
    }

    fn parse_function(&mut self) -> Result<Statement, ParserError> {
        self.expect(TokenKind::Fn)?;
        let name = self.expect_identifier()?;

        self.expect(TokenKind::LeftParen)?;
        
        let mut params = Vec::new();

        loop {
            if let Some(Token { kind: TokenKind::RightParen, .. }) = self.tokens.peek() { break; }
            params.push(self.expect_identifier()?);

            match self.tokens.peek() {
                Some(token) => match token.kind {
                    TokenKind::Comma => self.tokens.next(),
                    TokenKind::RightParen => break,
                    _ => return Err(ParserError::new(
                        ParserErrorKind::UnexpectedToken(token.kind.clone()),
                        token.span.clone()
                    ))
                },
                None => return Err(ParserError::new(
                    ParserErrorKind::UnexpectedEndOfFile,
                    self.current_span()
                ))
            };
        }

        self.expect(TokenKind::RightParen)?;

        let body = self.parse_body()?;

        Ok(Statement::Function {
            name,
            params,
            body
        })
    }
    
    fn parse_declaration(&mut self) -> Result<Statement, ParserError> {
        self.expect(TokenKind::Let)?;
        let name = self.expect_identifier()?;
        self.expect(TokenKind::Equal)?;
        let value = self.parse_expression()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Statement::Declaration { name, value })
    }

    fn parse_return(&mut self) -> Result<Statement, ParserError> {
        self.expect(TokenKind::Return)?;
        let expression = self.parse_expression()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Statement::Return(expression))
    }

    fn parse_if(&mut self) -> Result<Statement, ParserError> {
        self.expect(TokenKind::If)?;
        self.expect(TokenKind::LeftParen)?;
        let condition = self.parse_expression()?;
        self.expect(TokenKind::RightParen)?;
        let then_branch = self.parse_body()?;

        let else_branch = if let Some(Token { kind: TokenKind::Else, .. }) = self.tokens.peek() {
            self.tokens.next();
            match self.tokens.peek() {
                Some(token) if token.kind == TokenKind::If => {
                    Some(ElseBranch::If(Box::new(self.parse_if()?)))
                },
                _ => Some(ElseBranch::Else(self.parse_body()?))
            }
        } else {
            None
        };

        Ok(Statement::If { condition, then_branch, else_branch })
    }

    fn parse_while(&mut self) -> Result<Statement, ParserError> {
        self.expect(TokenKind::While)?;
        self.expect(TokenKind::LeftParen)?;
        let condition = self.parse_expression()?;
        self.expect(TokenKind::RightParen)?;

        let body = self.parse_body()?;

        Ok(Statement::While { condition, body })
    }

    fn parse_assignment(&mut self) -> Result<Statement, ParserError> {
        let name = self.expect_identifier()?;
        self.expect(TokenKind::Equal)?;
        let value = self.parse_expression()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Statement::Assignment { name, value })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.tokens.peek().map(|t| &t.kind) {
            Some(TokenKind::Fn) => self.parse_function(),
            Some(TokenKind::Let) => self.parse_declaration(),
            Some(TokenKind::Return) => self.parse_return(),
            Some(TokenKind::If) => self.parse_if(),
            Some(TokenKind::While) => self.parse_while(),
            Some(TokenKind::Identifier(_)) => {
                let next = self.tokens.clone().nth(1);
                if let Some(Token { kind: TokenKind::Equal, ..}) = next {
                    self.parse_assignment()
                } else {
                    let expr = self.parse_expression()?;
                    self.expect(TokenKind::Semicolon)?;
                    Ok(Statement::Expression(expr))
                }
            },
            _ => {
                let expr = self.parse_expression()?;
                self.expect(TokenKind::Semicolon)?;
                Ok(Statement::Expression(expr))
            }
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut statements = Vec::new();

        while let Some(token) = self.tokens.peek() {
            if token.kind == TokenKind::Eof { break }
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }
}
