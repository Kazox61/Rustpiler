use crate::tokenizer::{Tokenizer, TokenType, KeywordType};

use std::fmt;
use std::ops::Deref;

enum UnaryOperator {
    Negation,
    BitwiseComplement,
    LogicalNegation
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::Negation => write!(f, "-"),
            UnaryOperator::BitwiseComplement => write!(f, "~"),
            UnaryOperator::LogicalNegation => write!(f, "!"),
        }
    }
}

fn convert_to_unary_operator(token_type: &TokenType) -> Option<UnaryOperator>{
    return match token_type {
        TokenType::Hyphen => Some(UnaryOperator::Negation),
        TokenType::Tilde => Some(UnaryOperator::BitwiseComplement),
        TokenType::ExclamationMark => Some(UnaryOperator::LogicalNegation),
        _ => None
    }
}

enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperator::Addition => write!(f, "+"),
            BinaryOperator::Subtraction => write!(f, "-"),
            BinaryOperator::Multiplication => write!(f, "*"),
            BinaryOperator::Division => write!(f, "/"),
            BinaryOperator::And => write!(f, "&&"),
            BinaryOperator::Or => write!(f, "||"),
            BinaryOperator::Equal => write!(f, "=="),
            BinaryOperator::NotEqual => write!(f, "!="),
            BinaryOperator::LessThan => write!(f, "<"),
            BinaryOperator::LessThanOrEqual => write!(f, "<="),
            BinaryOperator::GreaterThan => write!(f, ">"),
            BinaryOperator::GreaterThanOrEqual => write!(f, ">="),
        }
    }
}

fn convert_to_binary_operator(token_type: &TokenType) -> Option<BinaryOperator>{
    return match token_type {
        TokenType::Plus => Some(BinaryOperator::Addition),
        TokenType::Hyphen => Some(BinaryOperator::Subtraction),
        TokenType::Asterisk => Some(BinaryOperator::Multiplication),
        TokenType::Slash => Some(BinaryOperator::Division),
        TokenType::And => Some(BinaryOperator::And),
        TokenType::Or => Some(BinaryOperator::Or),
        TokenType::Equal => Some(BinaryOperator::Equal),
        TokenType::NotEqual => Some(BinaryOperator::NotEqual),
        TokenType::LessThan => Some(BinaryOperator::LessThan),
        TokenType::LessThanOrEqual => Some(BinaryOperator::LessThanOrEqual),
        TokenType::GreaterThan => Some(BinaryOperator::GreaterThan),
        TokenType::GreaterThanOrEqual => Some(BinaryOperator::GreaterThanOrEqual),
        _ => None
    }
}

enum Factor {
    Expression(Box<Expression>),
    UnOpFactor(UnaryOperator, Box<Factor>),
    Constant(isize),
    Id(String)
}

struct FactorTuple {
    binary_operator: BinaryOperator, //BinaryOperator can only be * and /
    factor: Factor
}

struct Term {
    factor: Factor,
    binary_factors: Vec<FactorTuple> //BinaryOperator can only be * and /
}

struct TermTuple {
    term: Term,
    binary_operator: BinaryOperator, //BinaryOperator can only be + and -
}

struct AdditiveExpression {
    term: Term,
    terms: Vec<TermTuple> //BinaryOperator can only be + and -
}

struct AdditiveTuple {
    expression: AdditiveExpression,
    binary_operator: BinaryOperator
}

struct RelationalExpression {
    expression: AdditiveExpression,
    expressions: Vec<AdditiveTuple>
}

struct RelationalTuple {
    expression: RelationalExpression,
    binary_operator: BinaryOperator
}

struct EqualityExpression {
    expression: RelationalExpression,
    expressions: Vec<RelationalTuple>
}

struct EqualityTuple {
    expression: EqualityExpression,
    binary_operator: BinaryOperator
}

struct LogicalAndExpression {
    expression: EqualityExpression,
    expressions: Vec<EqualityTuple>
}

struct LogicalAndTuple {
    expression: LogicalAndExpression,
    binary_operator: BinaryOperator
}

struct LogicalOrExpression {
    expression: LogicalAndExpression,
    expressions: Vec<LogicalAndTuple>
}

enum Expression {
    LogicalOrExpression(LogicalOrExpression),
    Expression(Box<Expression>)
}

enum Statement {
    Return(Expression),
    Declaration(String, Option<Expression>),
    Expression(Expression)
}

struct Function {
    name: String,
    statements: Vec<Statement>
}

struct Program {
    functions: Vec<Function>
}

pub struct Parser {
    tokenizer: Tokenizer
}

impl Parser {
    pub fn new(tokenizer: Tokenizer) -> Parser {
        Parser {
            tokenizer
        }
    }

    pub fn parse(&mut self) {
        let result = self.parse_program();
        match result {
            Err(err) => println!("Error while Parsing, {:?}", err),
            Ok(prog) => {
                debug(prog);
                println!("Parsed Program successfully!")
            }
        }
    }

    fn parse_program(&mut self) -> Result<Program, String> {
        let function =  self.parse_function();
        return match function {
            Err(err) => Err(err),
            Ok(function) => Ok(Program {
                functions: vec![function],
            })
        }
    }

    fn parse_function(&mut self) -> Result<Function, String> {
        let token: &TokenType = match self.tokenizer.consume() {
            Some(value) => value.clone(),
            None => return Err(String::from("164"))
        };

        match token {
            TokenType::Keyword(keyword_type) => {
                match keyword_type {
                    KeywordType::Int => {}
                    _ => return Err(format!("Can't parse Function. There needs to be a Keyword type Int. Got Keyword Type <{}>", keyword_type))
                }
            }
            _ => return Err(format!("Can't parse Function. There needs to be a Keyword type Int. Got TokenType <{}>", token))
        }

        let token: &TokenType = match self.tokenizer.consume() {
            Some(value) => value.clone(),
            None => return Err(String::from("179"))
        };

        let id: String = match token {
            TokenType::Identifier(value) => value.clone(),
            _ => return Err(String::from("184"))
        };

        let token: &TokenType = match self.tokenizer.consume() {
            Some(value) => value.clone(),
            None => return Err(String::from("189"))
        };

        match token {
            TokenType::OpenParenthesis => {}
            _ => return Err(String::from("194"))
        }

        let token: &TokenType = match self.tokenizer.consume() {
            Some(value) => value.clone(),
            None => return Err(String::from("199"))
        };

        match token {
            TokenType::CloseParenthesis => {}
            _ => return Err(String::from("204"))
        }

        let token: &TokenType = match self.tokenizer.consume() {
            Some(value) => value.clone(),
            None => return Err(String::from("209"))
        };

        match token {
            TokenType::OpenBrace => {}
            _ => return Err(String::from("214"))
        }

        let mut statements: Vec<Statement> = Vec::new();

        let mut peek_token = match self.tokenizer.look_ahead(0) {
            None => return Err(String::from("No more Tokens available")),
            Some(token_type) => token_type.clone()
        };

        while *peek_token != TokenType::CloseBrace {
            let result = self.parse_statement();
            match result {
                Err(err) => return Err(err),
                Ok(statement) => statements.push(statement)
            }

            peek_token = match self.tokenizer.look_ahead(0) {
                None => return Err(String::from("No more Tokens available")),
                Some(token_type) => token_type.clone()
            };
        }

        let token: &TokenType = match self.tokenizer.consume() {
            Some(value) => value.clone(),
            None => return Err(String::from("221"))
        };

        match token {
            TokenType::CloseBrace => {}
            _ => return Err(String::from("226"))
        }

        return Ok(Function{
            name: id,
            statements,
        });
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.try_parse_return_statement() {
            Err(err) => return Err(err),
            Ok(option) => match option {
                None => {},
                Some(statement) => return Ok(statement)
            }
        }

        match self.try_parse_declaration_statement() {
            Err(err) => return Err(err),
            Ok(option) => match option {
                None => {},
                Some(statement) => {
                    println!("Parsed Declaration successfully!");
                    return Ok(statement); }
            }
        }

        return match self.parse_expression() {
            Err(err) => Err(err),
            Ok(expression) => Ok(Statement::Expression(Expression::Expression(Box::new(expression))))
        }
    }

    fn try_parse_return_statement(&mut self) -> Result<Option<Statement>, String> {
        println!("Try Parsing Return Statement");
        let token = match self.tokenizer.look_ahead(0) {
            Some(value) => value.clone(),
            None => return Err(String::from("No more Tokens available"))
        };

        match token {
            TokenType::Keyword(keyword_type) => {
                match keyword_type {
                    KeywordType::Return => self.tokenizer.consume(),
                    _ => return Ok(None)
                }
            }
            _ => return Ok(None)
        };

        let expression = match self.parse_logical_or_expression() {
            Err(err) => return Err(err),
            Ok(expression) => expression
        };

        let token: &TokenType = match self.tokenizer.consume() {
            Some(value) => value.clone() ,
            None => return Err(String::from("No more Tokens available"))
        };

        return match token {
            TokenType::Semicolon => Ok(Some(Statement::Return(Expression::LogicalOrExpression(expression)))),
            _ => Err(String::from("273"))
        };
    }

    fn try_parse_declaration_statement(&mut self) -> Result<Option<Statement>, String> {
        println!("Try Parsing Declaration Statement");
        let token = match self.tokenizer.look_ahead(0) {
            Some(value) => value.clone(),
            None => return Err(String::from("No more Tokens available"))
        };

        match token {
            TokenType::Keyword(keyword_type) => {
                match keyword_type {
                    KeywordType::Int => self.tokenizer.consume(),
                    _ => {
                        return Ok(None);
                    }
                }
            }

            _ => return Ok(None)
        };

        let next_token = match self.tokenizer.consume() {
            Some(value) => value.clone(),
            None => return Err(String::from("No more Tokens available"))
        };

        let variable_name = match next_token {
            TokenType::Identifier(identifier) => identifier.clone(),
            _ => return Err(String::from("There needs to come a Token type Identifier (Variable Name)"))
        };

        let peek_token = match self.tokenizer.look_ahead(0) {
            Some(value) => value.clone(),
            None => return Err(String::from("No more Tokens available"))
        };

        return match peek_token {
            TokenType::Semicolon => {
                self.tokenizer.consume();
                Ok(Some(Statement::Declaration(variable_name,None)))
            },
            TokenType::Assignment => {
                self.tokenizer.consume();
                match self.parse_logical_or_expression() {
                    Err(err) => Err(err),
                    Ok(expression) => {
                        self.tokenizer.consume();
                        return Ok(Some(Statement::Declaration(variable_name, Some(Expression::LogicalOrExpression(expression)))));
                    }
                }
            },
            _ => Err(format!("There needs to be a Token of type Semicolon or Assignment! Got {}", peek_token))
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, String> {
        let peek_token = match self.tokenizer.look_ahead(0) {
            Some(value) => value.clone(),
            None => return Err(String::from("No more Tokens available"))
        };

        match peek_token {
            TokenType::Identifier(_) => {
                let next_token = match self.tokenizer.look_ahead(1) {
                    None => return Err(String::from("No more Tokens available. Need a Assignment")),
                    Some(token) => token
                };
                match next_token {
                    TokenType::Assignment => {
                        self.tokenizer.consume_multiple(2);
                        return self.parse_expression();
                    }
                    _ => { }
                }
            }
            _ => { }
        };

        match self.parse_logical_or_expression() {
            Err(err) => Err(err),
            Ok(expression) => Ok(Expression::LogicalOrExpression(expression))
        }
    }

    fn parse_logical_or_expression(&mut self) -> Result<LogicalOrExpression, String> {
        let first_expression = match self.parse_logical_and_expression() {
            Err(err) => return Err(err),
            Ok(expression) => expression
        };

        let mut peek_token = match self.tokenizer.look_ahead(0) {
            None => return Err(String::from("There is no Token anymore")),
            Some(token) => token.clone()
        };

        let mut logical_and_tuples: Vec<LogicalAndTuple> = Vec::new();

        while *peek_token == TokenType::Or {
            let next_token = match self.tokenizer.consume() {
                None => return Err(String::from("There is no Token anymore")),
                Some(token) => token.clone()
            };

            let binary_operator = convert_to_binary_operator(next_token).unwrap(); // Can't be None
            let expression = match self.parse_logical_and_expression() {
                Err(err) => return Err(err),
                Ok(term) => term
            };
            logical_and_tuples.push(LogicalAndTuple {
                expression,
                binary_operator,
            });
            peek_token = match self.tokenizer.look_ahead(0) {
                None => return Err(String::from("There is no Token anymore")),
                Some(token) => token.clone()
            }
        }
        return Ok(LogicalOrExpression {
            expression: first_expression,
            expressions: logical_and_tuples,
        });
    }

    fn parse_logical_and_expression(&mut self) -> Result<LogicalAndExpression, String> {
        let first_expression = match self.parse_equality_expression() {
            Err(err) => return Err(err),
            Ok(expression) => expression
        };

        let mut peek_token = match self.tokenizer.look_ahead(0) {
            None => return Err(String::from("There is no Token anymore")),
            Some(token) => token.clone()
        };

        let mut equality_tuples: Vec<EqualityTuple> = Vec::new();

        while *peek_token == TokenType::And {
            let next_token = match self.tokenizer.consume() {
                None => return Err(String::from("There is no Token anymore")),
                Some(token) => token.clone()
            };

            let binary_operator = convert_to_binary_operator(next_token).unwrap(); // Can't be None
            let expression = match self.parse_equality_expression() {
                Err(err) => return Err(err),
                Ok(term) => term
            };
            equality_tuples.push(EqualityTuple {
                expression,
                binary_operator,
            });
            peek_token = match self.tokenizer.look_ahead(0) {
                None => return Err(String::from("There is no Token anymore")),
                Some(token) => token.clone()
            }
        }
        return Ok(LogicalAndExpression {
            expression: first_expression,
            expressions: equality_tuples,
        });
    }

    fn parse_equality_expression(&mut self) -> Result<EqualityExpression, String> {
        let first_expression = match self.parse_relational_expression() {
            Err(err) => return Err(err),
            Ok(expression) => expression
        };

        let mut peek_token = match self.tokenizer.look_ahead(0) {
            None => return Err(String::from("There is no Token anymore")),
            Some(token) => token.clone()
        };

        let mut relational_tuples: Vec<RelationalTuple> = Vec::new();

        while *peek_token == TokenType::Equal || *peek_token == TokenType::NotEqual {
            let next_token = match self.tokenizer.consume() {
                None => return Err(String::from("There is no Token anymore")),
                Some(token) => token.clone()
            };

            let binary_operator = convert_to_binary_operator(next_token).unwrap(); // Can't be None
            let expression = match self.parse_relational_expression() {
                Err(err) => return Err(err),
                Ok(term) => term
            };
            relational_tuples.push(RelationalTuple {
                expression,
                binary_operator,
            });
            peek_token = match self.tokenizer.look_ahead(0) {
                None => return Err(String::from("There is no Token anymore")),
                Some(token) => token.clone()
            }
        }
        return Ok(EqualityExpression {
            expression: first_expression,
            expressions: relational_tuples,
        });
    }

    fn parse_relational_expression(&mut self) -> Result<RelationalExpression, String> {
        let first_expression = match self.parse_additive_expression() {
            Err(err) => return Err(err),
            Ok(expression) => expression
        };

        let mut peek_token = match self.tokenizer.look_ahead(1) {
            None => return Err(String::from("There is no Token anymore")),
            Some(token) => token.clone()
        };

        let mut additive_tuples: Vec<AdditiveTuple> = Vec::new();

        while (
            *peek_token == TokenType::LessThan ||
                *peek_token == TokenType::GreaterThan ||
                *peek_token == TokenType::LessThanOrEqual ||
                *peek_token == TokenType::GreaterThanOrEqual
        ) {
            let next_token = match self.tokenizer.consume() {
                None => return Err(String::from("There is no Token anymore")),
                Some(token) => token.clone()
            };

            let binary_operator = convert_to_binary_operator(next_token).unwrap(); // Can't be None
            let expression = match self.parse_additive_expression() {
                Err(err) => return Err(err),
                Ok(term) => term
            };
            additive_tuples.push(AdditiveTuple {
                expression,
                binary_operator,
            });
            peek_token = match self.tokenizer.consume() {
                None => return Err(String::from("There is no Token anymore")),
                Some(token) => token.clone()
            }
        }
        return Ok(RelationalExpression {
            expression: first_expression,
            expressions: additive_tuples,
        });
    }

    fn parse_additive_expression(&mut self) -> Result<AdditiveExpression, String> {
        let first_term = match self.parse_term() {
            Err(err) => return Err(err),
            Ok(term) => term
        };

        let mut peek_token = match self.tokenizer.look_ahead(0) {
            None => return Err(String::from("There is no Token anymore")),
            Some(token) => token.clone()
        };

        let mut terms: Vec<TermTuple> = Vec::new();

        while *peek_token == TokenType::Plus || *peek_token == TokenType::Hyphen {
            let next_token = match self.tokenizer.consume() {
                None => return Err(String::from("There is no Token anymore")),
                Some(token) => token.clone()
            };

            let binary_operator = convert_to_binary_operator(next_token).unwrap(); // Can't be None
            let term = match self.parse_term() {
                Err(err) => return Err(err),
                Ok(term) => term
            };
            terms.push(TermTuple {
                term,
                binary_operator,
            });
            peek_token = match self.tokenizer.look_ahead(0) {
                None => return Err(String::from("There is no Token anymore")),
                Some(token) => token.clone()
            }
        }
        return Ok(AdditiveExpression {
            term: first_term,
            terms
        });
    }

    fn parse_term(&mut self) -> Result<Term, String> {
        let first_factor = match self.parse_factor() {
            Err(err) => return Err(err),
            Ok(factor) => factor
        };
        let mut peek_token = match self.tokenizer.look_ahead(0) {
            None => return Err(String::from("There is no Token anymore")),
            Some(token) => token
        };

        let mut factor_tuples: Vec<FactorTuple> = Vec::new();

        while *peek_token == TokenType::Asterisk || *peek_token == TokenType::Slash {
            let next_token = match self.tokenizer.consume() {
                None => return Err(String::from("There is no Token anymore")),
                Some(token) => token.clone()
            };

            let binary_operator = convert_to_binary_operator(next_token).unwrap(); // Can't be None
            let factor = match self.parse_factor() {
                Err(err) => return Err(err),
                Ok(factor) => factor
            };
            factor_tuples.push(FactorTuple {
                binary_operator,
                factor,
            });
            peek_token = match self.tokenizer.look_ahead(0) {
                None => return Err(String::from("There is no Token anymore")),
                Some(token) => token
            }

        }
        return Ok(Term {
            factor: first_factor,
            binary_factors: factor_tuples,
        });
    }

    fn parse_factor(&mut self) -> Result<Factor, String> {
        let next_token = match self.tokenizer.consume() {
            None => return Err(String::from("There is no Token anymore")),
            Some(token) => token.clone()
        };

        //handle "(" <exp> ")"
        if *next_token == TokenType::OpenParenthesis {
            let exp_result = self.parse_expression();
            let exp = match exp_result {
                Err(err) => return Err(err),
                Ok(exp) => exp
            };
            let new_token =  match self.tokenizer.consume() {
                None => return Err(String::from("There is no Token anymore")),
                Some(token) => token
            };
            if *new_token != TokenType::CloseParenthesis {
                return Err(String::from("There needs to be a <CloseParenthesis> Token, but there isn't!"));
            }
            return Ok(Factor::Expression(Box::new(exp)));
        }

        //handle <unary_op> <factor>
        let res = convert_to_unary_operator(&next_token);
        match res {
            None => {}
            Some(unOp) => {
                let res_factor = self.parse_factor();
                return match res_factor {
                    Err(err) => Err(err),
                    Ok(factor) => Ok(Factor::UnOpFactor(unOp, Box::new(factor)))
                }
            }
        }

        return match next_token {
            //handle <int>
            TokenType::IntegerLiteral(constant_string) => {
                match constant_string.parse() {
                    Ok(num) => Ok(Factor::Constant(num)),
                    Err(_) => return Err(String::from("Can't parse the Integer!"))
                }
            }
            //handle Id
            TokenType::Identifier(identifier) => Ok(Factor::Id(identifier.clone())),
            _ => return Err(format!("Expression is invalid. Got TokenType <{}>", next_token))
        };
    }
}

fn debug(program: Program) {
    println!("\n\nDEBUGGER\n________");
    for function in program.functions {
        println!("Fun INT {}:", function.name);
        println!("\tparams: ()");
        println!("\tbody:");
        for statement in function.statements {
            debug_statement(&statement);
            print!("\n");
        }
    }
    println!("\n\n");
}

fn debug_statement(statement: &Statement) {
    match statement {
        Statement::Declaration(variable_name, expression_option) => debug_declaration(variable_name, expression_option),
        Statement::Expression(expression) => debug_expression(&expression),
        Statement::Return(expression) => {
            print!("\t\tReturn ");
            debug_expression(expression);
        }
    }
}

fn debug_declaration(variable_name: &String, expression_option: &Option<Expression>) {
    print!("\t\tint {}", variable_name);
    match expression_option {
        None => {}
        Some(expression) => {
            print!(" = ");
            debug_expression(&expression)
        }
    }
}

fn debug_expression(expression_type: &Expression) {
    match expression_type {
        Expression::LogicalOrExpression(expression) => debug_logical_or_expression(expression),
        Expression::Expression(box_expression) => debug_expression(box_expression.deref())
    }
}

fn debug_logical_or_expression(expression_type: &LogicalOrExpression) {
    debug_logical_and_expression(&expression_type.expression);
    for tuple in &expression_type.expressions {
        print!(" {} ", tuple.binary_operator);
        debug_logical_and_expression(&tuple.expression);
    }
}

fn debug_logical_and_expression(expression_type: &LogicalAndExpression) {
    debug_equality_expression(&expression_type.expression);
    for tuple in &expression_type.expressions {
        print!(" {} ", tuple.binary_operator);
        debug_equality_expression(&tuple.expression);
    }
}

fn debug_equality_expression(expression_type: &EqualityExpression) {
    debug_relational_expression(&expression_type.expression);
    for tuple in &expression_type.expressions {
        print!(" {} ", tuple.binary_operator);
        debug_relational_expression(&tuple.expression);
    }
}

fn debug_relational_expression(expression_type: &RelationalExpression) {
    debug_additive_expression(&expression_type.expression);
    for tuple in &expression_type.expressions {
        print!(" {} ", tuple.binary_operator);
        debug_additive_expression(&tuple.expression);
    }
}

fn debug_additive_expression(expression: &AdditiveExpression) {
    debug_term(&expression.term);
    for term_tuple in &expression.terms {
        print!(" {} ", term_tuple.binary_operator);
        debug_term(&term_tuple.term);
    }
}

fn debug_term(term: &Term) {
    debug_factor(&term.factor);
    for binary_factor in &term.binary_factors {
        print!(" {} ", binary_factor.binary_operator);
        debug_factor(&binary_factor.factor);
    }
}

fn debug_factor(factor: &Factor) {
    match factor {
        Factor::Constant(constant) => print!("{}", constant),
        Factor::UnOpFactor(unary_operator, factor_box) => {
            print!("{}", unary_operator);
            let factor = factor_box.deref();
            debug_factor(factor);
        }
        Factor::Expression(expression_box) => {
            print!("(");
            debug_expression(expression_box.deref());
            print!(")");
        }
        Factor::Id(id) => print!("{}", id)
    }
}