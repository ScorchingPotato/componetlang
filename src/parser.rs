
use std::fmt::format;

use crate::lexer::{Token,TokenType};

#[derive(Default)]
pub struct ParseError {
    pub line: usize,
    pub column: usize,
    pub message: String,
    pub found: String,
}
impl ParseError {
    pub fn new(line: usize, column: usize, message: String, found: String) -> Self {
        Self {
            line,
            column,
            message,
            found,
        }
    }
    pub fn throw(&self) -> String {
        let f = if self.found.is_empty() {
            "".to_string()
        } else {
            format!(" (found: {})", self.found)
        };
        format!("Parse error at line {}, column {}: {} {}", 
            self.line, self.column, self.message, f)
    }
}


#[derive(Clone,Debug,PartialEq)]
pub enum Type {
    Int,
    Float,
    Str,
    Bool,
    Option(Vec<Type>),
    Arr(Box<Option<Type>>),
    Custom(String),
    None
}
#[derive(Clone,Debug,PartialEq)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Ne, Lt, Lte, Gt, Gte,
    And, Or,
}
#[derive(Clone,Debug,PartialEq)]
pub enum UnaryOp {
    Not, Neg,
}
#[derive(Clone,Debug,PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    None
}
#[derive(Clone,Debug,PartialEq)]
pub struct MatchArm {
    pub pattern: Expr,
    pub body: Vec<Stmt>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDef {
    pub visibility: Visibility,
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Vec<Stmt>,
}



#[derive(Debug, Clone, PartialEq,)]
pub struct FieldDef {
    pub name: String,
    pub components: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComponentDef {
    pub name: String,
    pub required_components: Vec<String>,
    pub values: Vec<ValueDecl>,
    pub functions: Vec<FunctionDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueDecl {
    pub visibility: Visibility,
    pub name: String,
    pub type_annotation: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub argtype: Option<Type>
}

#[derive(Clone,Debug,PartialEq)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    Call {
        callee: String,
        args: Vec<Expr>,
    },
    MemberAccess {
        object: Box<Expr>,
        member: String,
    },
    MemberCall {
        object: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },
    IndexAccess {
        object: Box<Expr>,
        index: Box<Expr>,
    },
    ArrayLiteral(Vec<Expr>)
}
#[derive(Clone,Debug,PartialEq)]
pub enum Stmt {
    VarDecl {
        name: String,
        type_annotation: Option<Type>,
        init: Expr,
    },
    Assignment {
        target: String,
        value: Expr,
    },
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },
    For {
        var_name: String,
        iterable: Expr,
        body: Vec<Stmt>,
    },
    Match {
        expr: Expr,
        arms: Vec<MatchArm>,
    },
    MemberAssignment {
        object: Expr,
        member: String,
        value: Expr,
    },
    IndexAssignment {
        object: Expr,
        index: Expr,
        value: Expr,
    },
    Return(Expr),
    Break,
    Expression(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Function(FunctionDef),
    Field(FieldDef),
    Component(ComponentDef),
    Enum(EnumDef),
    Statement(Stmt),
}
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize
}

impl Parser {
    pub fn new(tokens:Vec<Token>) -> Self {
        Self {tokens,current:0}
    }

    pub fn parse(&mut self) -> Result<Program,ParseError> {
        let mut items = Vec::new();

        while !self.is_at_end() {
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(e) => return Err(e)
            }
        }
        

        Ok(Program { items })
    }

    fn parse_item(&mut self) -> Result<Item,ParseError> {
        let vis = if self.match_token(&TokenType::Public) {
            Visibility::Public
        } else {
            Visibility::Private
        };
        match &self.peek().tokenType {
            TokenType::Function => Ok(Item::Function(self.parse_function(vis)?)),
            TokenType::Field => Ok(Item::Field(self.parse_field()?)),
            TokenType::Component => Ok(Item::Component(self.parse_component()?)),
            TokenType::Enum => Ok(Item::Enum(self.parse_enum()?)),

            _ => Err(ParseError { line: self.peek().span.line, column: self.peek().span.column, message: "Unaccessible item, expected: 'field','comp','func','enum'".to_string(), found:  format!("{:?}",self.peek().tokenType)})
        }
    }

    fn parse_function(&mut self, vis:Visibility) -> Result<FunctionDef,ParseError> {
        self.consume(&TokenType::Function, "Expected keywoard 'func'")?;

        let rettype = if self.check(&TokenType::Colon) {
            Some(self.parse_type_annot()?)
        } else {
            None
        };
        let name = self.consume_identifier("Expected function name")?;
        self.consume(&TokenType::LParen, "Expected '(' after function name")?;

        let mut parameters = Vec::new();
        if !self.check(&TokenType::LParen) {
            loop {
                let paramname = self.consume_identifier("Expected parameter name")?;
                let paramtype = if self.check(&TokenType::Colon) {
                    Some(self.parse_type_annot()?)
                } else {
                    None
                };

                parameters.push(Parameter {
                    name: paramname,
                    type_annotation: paramtype
                });
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::RParen, "Expected ')' after parameters")?;
        self.consume(&TokenType::LBrace, "Expected '{' after function signature")?;
        let body = self.parse_statement_block()?;
        self.consume(&TokenType::RBrace, "Expected '}' after function body")?;
        
        Ok(FunctionDef {
            visibility: vis,
            name,
            return_type:rettype,
            parameters,
            body,
        })

    }

    fn parse_field(&mut self) -> Result<FieldDef,ParseError> {
        self.consume(&TokenType::Field, "Expected 'field' keyworad to declare a new field")?;
        let name = self.consume_identifier("Expected field name after keywoard 'field'")?;
        self.consume(&TokenType::LBrace, "Expected '{' after field signature")?;

        let components = self.parse_use()?;
        self.consume(&TokenType::RBrace, "Expected '}' after field body")?;
        Ok(FieldDef { name, components })
    }

    fn parse_use(&mut self) -> Result<Vec<String>,ParseError> {
        self.consume(&TokenType::Use, "Expected keywoard 'use'")?;
        self.consume(&TokenType::LBrace, "Expected '{' after 'use' keywoard")?;
        let mut components = Vec::new();
        loop {
            components.push(self.consume_identifier("Expected component")?);
            if !self.match_token(&TokenType::Comma) {
                break
            }
        }
        self.consume(&TokenType::RBrace, "Expected '}' after components")?;
        Ok(components)
    }

    fn parse_component(&mut self) -> Result<ComponentDef,ParseError> {
        self.consume(&TokenType::Component, "Expected 'comp'")?;
        let name = self.consume_identifier("Expected component name")?;
        self.consume(&TokenType::LBrace,"Expected '{' after component signature")?;
        let reqc = if self.check(&TokenType::Require) {
            self.parse_require()?
        } else {Vec::new()};
        let values = if self.check(&TokenType::Value) {
            self.parse_values()?
        } else {Vec::new()};
        let methods = if self.check(&TokenType::Impliment) {
            self.parse_impliments()?
        } else {Vec::new()};
        self.consume(&TokenType::RBrace, "Expected '}' after component body")?;
        Ok(ComponentDef { name, required_components: reqc, values, functions: methods })
    }

    fn parse_require(&mut self) -> Result<Vec<String>,ParseError> {
        self.consume(&TokenType::Require, "Expected keywoard 'req'")?;
        self.consume(&TokenType::LBrace, "Expected '{' after 'req' keywoard")?;
        let mut components = Vec::new();
        loop {
            components.push(self.consume_identifier("Expected required component")?);
            if !self.match_token(&TokenType::Comma) {
                break;
            }
        }
        self.consume(&TokenType::RBrace, "Expected '}' after require body")?;
        Ok(components)
    }

    fn parse_values(&mut self) -> Result<Vec<ValueDecl>,ParseError> {
        self.consume(&TokenType::Value, "Expected keywoard 'val'")?;
        self.consume(&TokenType::LBrace, "Expected '{' after keyword 'val'")?;
        let mut values = Vec::new();
        loop {
            if self.check(&TokenType::RBrace) {
                break
            }

            let vis = if self.check(&TokenType::Public) {
                self.consume(&TokenType::Public,"Expected keywoard 'pub'")?;
                Visibility::Public
            } else {
                Visibility::Private
            };
            let name= self.consume_identifier("Expected value name")?;
            let vtype = if self.check(&TokenType::Colon) {
                Some(self.parse_type_annot()?)
            } else {None};

            values.push(ValueDecl {
                visibility: vis,
                name,
                type_annotation: vtype,

            })
        }
        self.consume(&TokenType::RBrace, "Expected '}' after value body")?;
        Ok(values)
    }

    fn parse_impliments(&mut self) -> Result<Vec<FunctionDef>,ParseError> {
        self.consume(&TokenType::Impliment, "Expected keywoard 'impl'")?;
        self.consume(&TokenType::LBrace, "Expected '{' after keyword 'impl'")?;
        let mut methods = Vec::new();
        loop {
            if self.check(&TokenType::RBrace) {break}

            let vis = if self.check(&TokenType::Public) {
                self.consume(&TokenType::Public, "Expected keyword 'pub'")?;
                Visibility::Public
            } else {
                Visibility::Private
            };
            methods.push(self.parse_function(vis)?);
        }
        self.consume(&TokenType::RBrace, "Expected '}' after impliment body")?;
        Ok(methods)
    }

    fn parse_enum(&mut self) -> Result<EnumDef,ParseError> {
        self.consume(&TokenType::Enum, "Expected keyword 'enum'")?;
        let name = self.consume_identifier("Expected enum name")?;
        self.consume(&TokenType::LBrace, "Expected '{' after enum signature")?;
        let mut variants = Vec::new();
        loop {
            let vname = self.consume_identifier("Expected variant name")?;
            let argtype = if self.check(&TokenType::LParen) {
                self.consume(&TokenType::LParen, "Expected '(' after enum variant")?;
                let v = self.parse_type_annot()?;
                self.consume(&TokenType::RParen, "Expected ')' after enum variant type argument")?;
                Some(v)
            } else {None};

            variants.push(EnumVariant {name:vname,argtype});

            if !self.match_token(&TokenType::Comma) {
                break
            }
        }
        
        self.consume(&TokenType::RBrace, "Expected '}' after enum body")?;
        Ok(EnumDef { name, variants })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match &self.peek().tokenType {
            TokenType::Match => self.parse_match_stmt(),
            TokenType::While => self.parse_while_stmt(),
            TokenType::For => self.parse_for_stmt(),
            TokenType::Return => self.parse_return_stmt(),
            TokenType::Break => {self.advance();Ok(Stmt::Break)}

            _ => {
                let expr = self.parse_expression()?;

                if self.match_token(&TokenType::Assign) || self.match_token(&TokenType::Declare) {
                    let decalaration = self.previous().tokenType == TokenType::Declare;
                    let vartype = if decalaration && self.check(&TokenType::Colon) {
                        Some(self.parse_type_annot()?)
                    } else {
                        None
                    };
                    let value = self.parse_expression()?;
                
                    match expr {
                        Expr::Identifier(name) => {
                            if decalaration {
                                Ok(Stmt::VarDecl { name, type_annotation: vartype, init: value })
                            } else {
                                Ok(Stmt::Assignment { target: name, value })
                            }
                        }
                        Expr::MemberAccess { object, member } => {
                            Ok(Stmt::MemberAssignment { object: *object, member, value })
                        }
                        Expr::IndexAccess { object, index } => {
                            Ok(Stmt::IndexAssignment { object: *object, index: *index, value })
                        }
                        _ => {
                        Err(ParseError::new(
                            self.peek().span.line,
                            self.peek().span.column,
                            "Invalid assignment target".to_string(),
                            format!("{:?}", expr)
                        ))
                    }
                    }
                } else {
                    Ok(Stmt::Expression(expr))
                }
            }
        }
    }

    fn parse_statement_block(&mut self) -> Result<Vec<Stmt>,ParseError> {
        let mut statements = Vec::new();

        while !self.is_at_end() && !self.check(&TokenType::RBrace) {
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }   

    fn parse_type_annot(&mut self) -> Result<Type, ParseError> {
        if self.check(&TokenType::Colon) {self.consume(&TokenType::Colon, "Expected ':' to assign type")?;}

        let t = match &self.peek().tokenType {
            TokenType::Int => {self.advance();Type::Int},
            TokenType::Float => {self.advance();Type::Float},
            TokenType::Str => {self.advance();Type::Str},
            TokenType::Bool => {self.advance();Type::Bool},
            TokenType::None => {self.advance();Type::None},
            TokenType::Identifier(name) => {
                let custom_name = name.clone();
                self.advance();
                Type::Custom(custom_name)
            },
            TokenType::Arr => {
                self.advance();
                let arrelmentype = if self.match_token(&TokenType::Colon) {
                    Box::new(Some(self.parse_type_annot()?))
                } else {
                    Box::new(None)
                };
                Type::Arr(arrelmentype)
            }
            TokenType::QMark => {
                self.consume(&TokenType::QMark,"")?;

                let mut options= Vec::new();
                if self.match_token(&TokenType::LParen) {
                    loop {
                        options.push(self.parse_type_annot()?);
                        if self.match_token(&TokenType::LParen) {
                            break
                        }
                    }
                } else {
                    options.push(self.parse_type_annot()?)
                }
                Type::Option(options)
            }

            _ => return Err(ParseError::new(
            self.peek().span.line,
            self.peek().span.column,
            "Invalid type annotation".to_string(),
            format!("{:?}", self.peek().tokenType)
        )),
        };

        Ok(t)
    } 

    fn parse_match_stmt(&mut self) -> Result<Stmt,ParseError> {
        self.consume(&TokenType::Match, "Expected 'match'")?;
        let expr = self.parse_expression()?;
        self.consume(&TokenType::LBrace, "Expected '{' after match signature")?;
        let mut arms = Vec::new();
        while !self.is_at_end() && !self.match_token(&TokenType::RBrace) {
            arms.push(self.parse_match_arm()?);
            if !self.check(&TokenType::RBrace) {self.consume(&TokenType::Comma, "Expected ',' after a match arm")?;}
            
        }
        self.consume(&TokenType::RBrace, "Expected '}' after match body")?;
        Ok(Stmt::Match { expr, arms })
    }

    fn parse_match_arm(&mut self) -> Result<MatchArm,ParseError> {
        let condition = self.parse_expression()?;
        self.consume(&TokenType::Arrow, "Expected '->' after match condition")?;
        let body = if self.match_token(&TokenType::LBrace) {
            let b = self.parse_statement_block()?;
            self.consume(&TokenType::RBrace,"Expected '}' after match arm body")?;
            b
        } else {
            vec![Stmt::Expression(self.parse_expression()?)]
        };
        Ok(MatchArm { pattern:condition, body })
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt,ParseError> {
        self.consume(&TokenType::While, "Exepected 'while'")?;
        let condition = self.parse_expression()?;
        self.consume(&TokenType::LBrace, "Expected '{' after while signarture")?;
        let body = self.parse_statement_block()?;
        self.consume(&TokenType::RBrace, "Expected '}' after while body")?;
        Ok(Stmt::While { condition, body })
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt,ParseError> {
        self.consume(&TokenType::For, "Expected 'for'")?;
        let var = self.consume_identifier("Expected iteration variable")?;
        self.consume(&TokenType::In, "Expected 'in' before iterator")?;
        let iter = self.parse_expression()?;
        self.consume(&TokenType::LBrace, "Expected '{' after 'for' loop signature")?;
        let body = self.parse_statement_block()?;
        self.consume(&TokenType::RBrace, "Expected '}' after for body")?;
        Ok(Stmt::For { var_name: var, iterable: iter, body })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt,ParseError> {
        self.consume(&TokenType::Return, "Expected 'return'")?;
        let expr = self.parse_expression()?;

        Ok(Stmt::Return(expr))
    }


    fn match_token(&mut self, token_type: &TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            &self.peek().tokenType == token_type
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }
    fn is_at_end(&self) -> bool {
        self.peek().tokenType == TokenType::EOF
    }


    fn consume(&mut self, token_type: &TokenType, message: &str) -> Result<(), ParseError> {
        if self.check(token_type) {
            self.advance();
            Ok(())
        } else {
            let token = self.peek();
            Err(ParseError::new(  
                token.span.line,
                token.span.column,
                message.to_string(),
                format!("{:?}", token.tokenType),
            ))
        }
    }

    fn consume_identifier(&mut self, message: &str) -> Result<String, ParseError> {
        if let TokenType::Identifier(name) = &self.peek().tokenType {
            let name = name.clone();
            self.advance();
            Ok(name)
        } else {
            let token = self.peek();
            Err(ParseError::new  (
                token.span.line,
                token.span.column,
                message.to_string(),
                format!("{:?}", token.tokenType),
            ))
        }
    }


    fn parse_expression(&mut self) -> Result<Expr,ParseError> {
        self.parse_or()
    }
    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_and()?;
        while self.match_token(&TokenType::Or) {
            let right = self.parse_and()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::Or,
                right: Box::new(right),
            };}
        Ok(expr)
    }
    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_equality()?;
        while self.match_token(&TokenType::And) {
            let right = self.parse_equality()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::And,
                right: Box::new(right),
            };}
        Ok(expr)
    }
    fn match_equality_op(&mut self) -> Option<BinaryOp> {
        if self.match_token(&TokenType::Eq) {
            Some(BinaryOp::Eq)
        } else if self.match_token(&TokenType::Ne) {
            Some(BinaryOp::Ne)
        } else {
            None
        }
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_comparison()?;   
        while let Some(op) = self.match_equality_op() {
            let right = self.parse_comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };}
        Ok(expr)
    }
    fn match_comparison_op(&mut self) -> Option<BinaryOp> {
        if self.match_token(&TokenType::Lt) {
            Some(BinaryOp::Lt)
        } else if self.match_token(&TokenType::Lte) {
            Some(BinaryOp::Lte)
        } else if self.match_token(&TokenType::Gt) {
            Some(BinaryOp::Gt)
        } else if self.match_token(&TokenType::Gte) {
            Some(BinaryOp::Gte)
        } else {
            None
        }
    }
    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_term()?;
        
        while let Some(op) = self.match_comparison_op() {
            let right = self.parse_term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    fn match_term_op(&mut self) -> Option<BinaryOp> {
        if self.match_token(&TokenType::Plus) {
            Some(BinaryOp::Add)
        } else if self.match_token(&TokenType::Minus) {
            Some(BinaryOp::Sub)
        } else {
            None
        }
    }
    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_factor()?;
        
        while let Some(op) = self.match_term_op() {
            let right = self.parse_factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    fn match_factor_op(&mut self) -> Option<BinaryOp> {
        if self.match_token(&TokenType::Multiply) {
            Some(BinaryOp::Mul)
        } else if self.match_token(&TokenType::Divide) {
            Some(BinaryOp::Div)
        } else if self.match_token(&TokenType::Modulus) {
            Some(BinaryOp::Mod)
        } else {
            None
        }
    }
    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;
        
        while let Some(op) = self.match_factor_op() {
            let right = self.parse_unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(&TokenType::Not) {
            let operand = self.parse_unary()?;
            Ok(Expr::Unary {
                op: UnaryOp::Not,
                operand: Box::new(operand),
            })
        } else if self.match_token(&TokenType::Minus) {
            let operand = self.parse_unary()?;
            Ok(Expr::Unary {
                op: UnaryOp::Neg,
                operand: Box::new(operand),
            })
        } else {
            self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> Result<Expr,ParseError> {
        let mut expr = match &self.peek().tokenType {
            TokenType::Number(n) => {
                let value = *n;
                self.advance();
                Expr::Literal(Literal::Number(value))
            }
            TokenType::String(s) => {
                let value = s.clone();
                self.advance();
                Expr::Literal(Literal::String(value))
            }
            TokenType::Boolean(b) => {
                let value = *b;
                self.advance();
                Expr::Literal(Literal::Boolean(value))
            }
            TokenType::None => {
                self.advance();
                Expr::Literal(Literal::None)
            }
            TokenType::Identifier(_) => {
                let name = self.consume_identifier("Expected identifier")?;
                Expr::Identifier(name)
            }
            TokenType::LParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.consume(&TokenType::RParen, "Expected ')' after expression")?;
                expr
            }
            TokenType::LBracket => {
                self.advance();
                let mut elements = Vec::new();
                if !self.check(&TokenType::RBracket) {
                    loop {
                        elements.push(self.parse_expression()?);
                        if !self.match_token(&TokenType::Comma) {
                            break;
                        }
                    }
                }
                self.consume(&TokenType::RBracket, "Expected ']' after array elements")?;
                Expr::ArrayLiteral(elements)
            }
            _ => return Err(ParseError::new(
                self.peek().span.line,
                self.peek().span.column,
                "Expected expression".to_string(),
                format!("{:?}", self.peek().tokenType)
            )),
        };

        loop {
            if self.match_token(&TokenType::Dot) {
                let member = self.consume_identifier("Expected member name after '.'")?;
                if self.match_token(&TokenType::LParen) {
                    let mut args = Vec::new();
                    if !self.check(&TokenType::RParen) {
                        loop {
                            args.push(self.parse_expression()?);
                            if !self.match_token(&TokenType::Comma) {
                                break;
                            }
                        }
                    }
                    self.consume(&TokenType::RParen, "Expected ')' after arguments")?;
                    expr = Expr::MemberCall {
                        object: Box::new(expr),
                        method: member,
                        args,
                    };
                } else {
                    expr = Expr::MemberAccess {
                        object: Box::new(expr),
                        member,
                    };
                }
            } else if self.match_token(&TokenType::RBracket) {
                let index = self.parse_expression()?;
                self.consume(&TokenType::LBracket, "Expected ']' after index")?;
                expr =  Expr::IndexAccess { object: Box::new(expr), index: Box::new(index) }
            } else if self.match_token(&TokenType::LParen) {
                if let Expr::Identifier(name) = expr {
                    let mut args = Vec::new();
                    if !self.check(&TokenType::RParen) {
                        loop {
                            args.push(self.parse_expression()?);
                            if !self.match_token(&TokenType::Comma) {
                                break;
                            }
                        }
                    }
                    self.consume(&TokenType::RParen, "Expected ')' after arguments")?;
                    expr = Expr::Call { callee: name, args };
                } else {
                    return Err(ParseError::new(
                        self.peek().span.line,
                        self.peek().span.column,
                        "Expected identifier for function call".to_string(),
                        format!("{:?}", expr)
                    ));
                }
            } else {
                break
            }
        }

        Ok(expr)
    }
}