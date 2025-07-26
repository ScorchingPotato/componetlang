use thiserror::Error;

#[derive(Debug,Clone,PartialEq)]
pub struct Token {
    pub tokenType: TokenType,
    pub span: Span,
    pub raw: String
}
impl Token {
    fn new(tokenType:TokenType,span:Span,raw:String) -> Self {
        Self {
            tokenType,
            span,
            raw
        }
    }
}

#[derive(Debug,Clone,PartialEq)]
pub enum TokenType {
    //ItemKeywords
    Field,
    Component,
    Function,
    Enum,
    //StatementKeywords
    Match,
    While,
    For,
    Use,
    Require,
    Value,
    Impliment,
    Public,

    //ControlFlowKeywords
    Return,
    Break,
    In,

    //TypeKeywords
    Int,
    Float,
    Bool,
    Str,
    Arr,

    //Operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulus,
    Assign,
    Declare,
    Eq,
    Ne,
    Gt,
    Lt,
    Gte,
    Lte,
    Not,
    And,
    Or,
    Arrow,

    //Punctuation
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    Colon,
    Dot,
    QMark,

    //Literals
    Number(f64),
    String(String),
    Boolean(bool),
    Identifier(String),
    None,

    //Other
    EOF,
    NewLine,
    WhiteSpace   
}
#[derive(Debug,Clone,PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}
impl Span {
    fn new(start:usize,end:usize,line:usize,column:usize) -> Self {
        Self {
            start,
            end,
            line,
            column
        }
    }
}
#[derive(Debug,Error)]
pub enum LexError {
    #[error("Unexpected character at line {line}, column {column}: {ch}")]
    UnexpectedChar { line: usize, column: usize, ch: char },
    #[error("Unterminated string literal at line {line}, column {column}")]
    UnterminatedString { line: usize, column: usize },
    #[error("Invalid number format at line {line}, column {column}: {text}")]
    InvalidNumber { line: usize, column: usize, text: String },
}

pub struct Lexer {
    input:String,
    position: usize,
    line: usize,
    column: usize
}

impl Lexer {
    pub fn new(input:String) -> Self {
        Self {
            input: input,
            position: 0,
            line: 0,
            column: 0
        }
    }
    pub fn tokenize(&mut self) -> Result<Vec<Token>,LexError> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.skip_whitespace();
            
            if self.is_at_end() {
                break;
            }

            
            let start_pos = self.position;
            let start_line = self.line;
            let start_column = self.column;
            
            let token_type = self.next_token()?;
            let end_pos = self.position;
            
            if !matches!(token_type, TokenType::WhiteSpace | TokenType::NewLine) {
                tokens.push(Token::new(token_type,Span::new(start_pos, end_pos, start_line, start_column),self.input[start_pos..end_pos].to_string()))
            }
        }

        tokens.push(Token::new(TokenType::EOF,Span::new(self.position,self.position,self.line,self.column),String::new()));

        Ok(tokens)
    }
    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }
    fn skip_whitespace(&mut self) {
        while matches!(self.peek(), ' ' | '\r' | '\t') {
            self.advance();
        }
    }
    fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        let ch = self.input.chars().nth(self.position).unwrap_or('\0');
        self.position += 1;
        self.column += 1;
        ch
    }
    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.input.chars().nth(self.position).unwrap_or('\0')
        }
    }
    fn peek_next(&self) -> char {
        if self.position + 1 >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.position + 1).unwrap_or('\0')
        }
    }
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.advance();
            true
        }
    }

    fn next_token(&mut self) -> Result<TokenType, LexError> {
        let c = self.advance();

        match c {
            '(' => Ok(TokenType::LParen),
            ')' => Ok(TokenType::RParen),
            '[' => Ok(TokenType::LBracket),
            ']' => Ok(TokenType::RBracket),
            '{' => Ok(TokenType::LBrace),
            '}' => Ok(TokenType::RBrace),
            ',' => Ok(TokenType::Comma),
            '.' => Ok(TokenType::Dot),
            ';' => Ok(TokenType::Semicolon),
            '&' => Ok(TokenType::And),
            '|' => Ok(TokenType::Or),
            '?' => Ok(TokenType::QMark),
            
            '+' => Ok(TokenType::Plus),
            '%' => Ok(TokenType::Modulus),
            '*' => Ok(TokenType::Multiply),
            '/' => Ok(TokenType::Divide),

            ':' => {
                if self.match_char('=') {
                    Ok(TokenType::Declare)
                } else {
                    Ok(TokenType::Colon)
                }
            }

            '#' => {
                while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                }
                Ok(TokenType::WhiteSpace)
            }

            '-' => {
                if self.match_char('>') {
                    Ok(TokenType::Arrow)
                } else {
                    Ok(TokenType::Minus)
                }
            }
            '=' => {
                if self.match_char('=') {
                    Ok(TokenType::Eq)
                } else {
                    Ok(TokenType::Assign)
                }
            }
            '>' => {
                if self.match_char('=') {
                    Ok(TokenType::Gte)
                } else {
                    Ok(TokenType::Gt)
                }
            }
            '<' => {
                if self.match_char('=') {
                    Ok(TokenType::Lte)
                } else {
                    Ok(TokenType::Lt)
                }
            }
            '!' => {
                if self.match_char('=') {
                    Ok(TokenType::Ne)
                } else {
                    Ok(TokenType::Not)
                }
            }

            '"' => self.string_literal(),

            '0'..='9' => self.number_literal(),

            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),

            ' ' | '\r' | '\t' => Ok(TokenType::WhiteSpace),
            '\n' => {
                self.line += 1;
                self.column = 1;
                Ok(TokenType::NewLine)
            }

            _ => Err(LexError::UnexpectedChar {line:self.line,column:self.column,ch:c})
        }
    }
    fn string_literal(&mut self) -> Result<TokenType, LexError> {
        let mut value = String::new();
        
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 1;
            }
            
            if self.peek() == '\\' {
                self.advance(); // consume backslash
                match self.peek() {
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    'r' => value.push('\r'),
                    '\\' => value.push('\\'),
                    '"' => value.push('"'),
                    _ => {
                        value.push('\\');
                        value.push(self.peek());
                    }
                }
            } else {
                value.push(self.peek());
            }
            self.advance();
        }
        
        if self.is_at_end() {
            return Err(LexError::UnterminatedString {
                line: self.line,
                column: self.column,
            });
        }
        
        // Consume closing quote
        self.advance();
        
        Ok(TokenType::String(value))
    }

    fn number_literal(&mut self) -> Result<TokenType, LexError> {
        let start_pos = self.position - 1;
        
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        
        // Look for decimal point
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance(); // consume '.'
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        
        let text = &self.input[start_pos..self.position];
        match text.parse::<f64>() {
            Ok(value) => Ok(TokenType::Number(value)),
            Err(_) => Err(LexError::InvalidNumber {
                line: self.line,
                column: self.column - text.len(),
                text: text.to_string(),
            }),
        }
    }
    fn identifier(&mut self) -> Result<TokenType, LexError> {
        let start_pos = self.position - 1;
        
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        
        let text = &self.input[start_pos..self.position];
        
        let token_type = match text {
            // Keywords
            "func" => TokenType::Function,
            "field" => TokenType::Field,
            "comp" => TokenType::Component,
            "enum" => TokenType::Enum,
            "while" => TokenType::While,
            "for" => TokenType::For,
            "match" => TokenType::Match,
            "return" => TokenType::Return,
            "break" => TokenType::Break,
            "in" => TokenType::In,
            "use" => TokenType::Use,
            "req" => TokenType::Require,
            "val" => TokenType::Value,
            "impl" => TokenType::Impliment,
            "pub" => TokenType::Public,

            //Types
            "int" => TokenType::Int,
            "float" => TokenType::Float,
            "str" => TokenType::Str,
            "arr" => TokenType::Arr,
            "bool" => TokenType::Bool,
            
            // Boolean literals
            "true" => TokenType::Boolean(true),
            "false" => TokenType::Boolean(false),
            "none" => TokenType::None,
            
            // Identifier
            _ => TokenType::Identifier(text.to_string()),
        };
        
        Ok(token_type)
    }
}