use std::fs;

#[derive(PartialEq)]
pub enum KeywordType {
    Int,
    Return,
    Break
}

#[derive(PartialEq)]
pub enum TokenType {
    Any,
    OpenBrace,
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    Semicolon,
    Keyword(KeywordType),
    Identifier(String), // [a-zA-Z]\w*
    IntegerLiteral(String), //[0-9]+
    Hyphen,
    Tilde,
    ExclamationMark,
    Plus,
    Asterisk,
    Slash,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Assignment
}

impl std::fmt::Display for KeywordType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KeywordType::Int => write!(f, "?Keyword? int"),
            KeywordType::Return => write!(f, "?Keyword? return"),
            KeywordType::Break => write!(f, "Keyword break"),
        }
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Any => write!(f, "Any Token"),
            TokenType::OpenBrace => write!(f, "{{"),
            TokenType::CloseBrace => write!(f, "}}"),
            TokenType::OpenParenthesis => write!(f, "("),
            TokenType::CloseParenthesis => write!(f, ")"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Keyword(keyword_type) => write!(f, "{}", keyword_type),
            TokenType::Identifier(value) => write!(f, "{}", value),
            TokenType::IntegerLiteral(value) => write!(f, "?IntegerLiteral? {}", value),
            TokenType::Hyphen => write!(f, "{}", "<->"),
            TokenType::Tilde => write!(f, "{}", "<~>"),
            TokenType::ExclamationMark => write!(f, "{}", "<!>"),
            TokenType::Plus => write!(f, "{}", "<+>"),
            TokenType::Asterisk => write!(f, "{}", "<*>"),
            TokenType::Slash => write!(f, "{}", "</>"),
            TokenType::And => write!(f, "{}", "<&&>"),
            TokenType::Or => write!(f, "{}", "<||>"),
            TokenType::Equal => write!(f, "{}", "<==>"),
            TokenType::NotEqual => write!(f, "{}", "<!=>"),
            TokenType::LessThan => write!(f, "{}", "<<>"),
            TokenType::LessThanOrEqual => write!(f, "{}", "<<=>"),
            TokenType::GreaterThan => write!(f, "{}", "<>>"),
            TokenType::GreaterThanOrEqual => write!(f, "{}", "<>=>"),
            TokenType::Assignment => write!(f, "{}", "<=>"),

        }
    }
}

pub struct Tokenizer {
    index: usize,
    tokens: Vec<TokenType>
}

impl Tokenizer {
    pub fn new(source_code_path: &str) -> Tokenizer {
        let tokens = Tokenizer::lex(source_code_path);
        Tokenizer {
            index: 0,
            tokens
        }
    }

    pub fn consume(&mut self) -> Option<&TokenType> {
        return self.consume_multiple(1);
    }

    pub fn consume_multiple(&mut self, amount: usize) -> Option<&TokenType> {
        let result = self.tokens.get(self.index);
        self.index += amount;
        return result;
    }

    pub fn look_ahead(&self, distance: usize) -> Option<&TokenType> {
        return self.tokens.get(self.index + distance);
    }

    fn lex(path: &str) -> Vec<TokenType> {
        let mut tokens: Vec<TokenType> = Vec::new();

        let source_code;

        match fs::read_to_string(path) {
            Ok(file_content) => source_code = file_content,
            Err(err) => {
                eprintln!("Error while trying to read the file: {}", err);
                return tokens;
            }
        }

        println!("The Source code has {} characters", source_code.chars().count());

        let mut characters = source_code.chars().peekable();

        while let Some(character) = characters.next() {
            match character {
                '&' => {
                    match characters.peek() {
                        None => {}
                        Some(&next_char) => {
                            if next_char == '&' {
                                characters.next();
                                tokens.push(TokenType::And)
                            }
                        }
                    }
                },
                '|' => {
                    match characters.peek() {
                        None => {}
                        Some(&next_char) => {
                            if next_char == '|' {
                                characters.next();
                                tokens.push(TokenType::Or)
                            }
                        }
                    }
                },
                '=' => {
                    match characters.peek() {
                        None => {}
                        Some(&next_char) => {
                            if next_char == '=' {
                                characters.next();
                                tokens.push(TokenType::Equal)
                            }
                            else {
                                tokens.push(TokenType::Assignment)
                            }
                        }
                    }
                },
                '!' => {
                    match characters.peek() {
                        None => {}
                        Some(&next_char) => {
                            if next_char == '=' {
                                characters.next();
                                tokens.push(TokenType::NotEqual)
                            }
                            else {
                                tokens.push(TokenType::ExclamationMark)
                            }
                        }
                    }
                },
                '<' => {
                    match characters.peek() {
                        None => {}
                        Some(&next_char) => {
                            if next_char == '=' {
                                characters.next();
                                tokens.push(TokenType::LessThanOrEqual)
                            }
                            else {
                                tokens.push(TokenType::LessThan);
                            }
                        }
                    }
                },
                '>' => {
                    match characters.peek() {
                        None => {}
                        Some(&next_char) => {
                            if next_char == '=' {
                                characters.next();
                                tokens.push(TokenType::GreaterThanOrEqual);
                            }
                            else {
                                tokens.push(TokenType::GreaterThan);
                            }
                        }
                    }
                },
                '{' => tokens.push(TokenType::OpenBrace),
                '}' => tokens.push(TokenType::CloseBrace),
                '(' => tokens.push(TokenType::OpenParenthesis),
                ')' => tokens.push(TokenType::CloseParenthesis),
                ';' => tokens.push(TokenType::Semicolon),
                '-' => tokens.push(TokenType::Hyphen),
                '~' => tokens.push(TokenType::Tilde),
                '+' => tokens.push(TokenType::Plus),
                '*' => tokens.push(TokenType::Asterisk),
                '/' => tokens.push(TokenType::Slash),
                _ => {
                    if character.is_alphabetic() {
                        let mut value = String::from(character);
                        loop {
                            if let Some(&next_char) = characters.peek() {
                                if !next_char.is_alphanumeric() {
                                    break;
                                }
                                value.push(next_char);
                                characters.next();
                            } else {
                                break;
                            }
                        }
                        match value.as_str() {
                            "int" => tokens.push(TokenType::Keyword(KeywordType::Int)),
                            "return" => tokens.push(TokenType::Keyword(KeywordType::Return)),
                            _ => tokens.push(TokenType::Identifier(value.clone()))
                        }
                        println!("Found Value: {}", value);
                    }
                    else if character.is_numeric() {
                        let mut value = String::from(character);
                        loop {
                            if let Some(&next_char) = characters.peek() {
                                if !next_char.is_numeric() {
                                    break;
                                }
                                value.push(next_char);
                                characters.next();
                            } else {
                                break;
                            }
                        }
                        tokens.push(TokenType::IntegerLiteral(value));
                    }
                    else {
                        match character {
                            '\r' => {},
                            '\n' => println!("Found Character without token: <\\n>, INVALID!"),
                            _ => println!("Found Character without token: <{}>, INVALID!", character)
                        }
                    }
                }
            }

        }

        return tokens;
    }
}