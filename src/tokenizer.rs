use std::error::Error;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BaseType {
    Number,
    String,
    Boolean,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
}

impl Value {
    pub fn base_type(&self) -> BaseType {
        match self {
            Value::Number(_) => BaseType::Number,
            Value::String(_) => BaseType::String,
            Value::Boolean(_) => BaseType::Boolean,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Plus,
    Minus,
    Star,
    Slash,
    DoubleAnd,
    DoubleOr,
    Equal,
    DoubleEqual,
    Exclamation,
    ExclamationEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    Let,
    If,
    Else,
    While,
    Colon,
    Semicolon,
    Number,
    String,
    Bool,
    Literal(Value),
    Variable(String),
}

#[derive(Debug)]
pub struct Token {
    pub line: usize,
    pub token_type: TokenType,
}

pub fn tokenize(program: &str) -> Result<Vec<Token>, Vec<Box<dyn Error>>> {
    let mut tokens = vec![];
    let mut errors = vec![];
    let mut chars = program.chars().peekable();
    let mut line = 1;

    loop {
        let Some(ch) = chars.next() else { break; };
        match ch {
            ' ' | '\t' | '\r' => {}
            '\n' => line += 1,
            '+' => tokens.push(Token {
                line,
                token_type: TokenType::Plus,
            }),
            '-' => tokens.push(Token {
                line,
                token_type: TokenType::Minus,
            }),
            '*' => tokens.push(Token {
                line,
                token_type: TokenType::Star,
            }),
            '/' => tokens.push(Token {
                line,
                token_type: TokenType::Slash,
            }),
            '(' => tokens.push(Token {
                line,
                token_type: TokenType::LeftParenthesis,
            }),
            ')' => tokens.push(Token {
                line,
                token_type: TokenType::RightParenthesis,
            }),
            '{' => tokens.push(Token {
                line,
                token_type: TokenType::LeftBrace,
            }),
            '}' => tokens.push(Token {
                line,
                token_type: TokenType::RightBrace,
            }),
            ':' => tokens.push(Token {
                line,
                token_type: TokenType::Colon,
            }),
            ';' => tokens.push(Token {
                line,
                token_type: TokenType::Semicolon,
            }),
            '&' => match chars.peek() {
                Some('&') => {
                    tokens.push(Token {
                        line,
                        token_type: TokenType::DoubleAnd,
                    });
                    chars.next();
                }
                _ => errors.push(format!("Invalid token {ch} at line {line}.").into()),
            },
            '|' => match chars.peek() {
                Some('|') => {
                    tokens.push(Token {
                        line,
                        token_type: TokenType::DoubleOr,
                    });
                    chars.next();
                }
                _ => errors.push(format!("Invalid token {ch} at line {line}.").into()),
            },
            '=' => match chars.peek() {
                Some('=') => {
                    tokens.push(Token {
                        line,
                        token_type: TokenType::DoubleEqual,
                    });
                    chars.next();
                }
                _ => {
                    tokens.push(Token {
                        line,
                        token_type: TokenType::Equal,
                    });
                }
            },
            '!' => match chars.peek() {
                Some('=') => {
                    tokens.push(Token {
                        line,
                        token_type: TokenType::ExclamationEqual,
                    });
                    chars.next();
                }
                _ => {
                    tokens.push(Token {
                        line,
                        token_type: TokenType::Exclamation,
                    });
                }
            },
            '<' => match chars.peek() {
                Some('=') => {
                    tokens.push(Token {
                        line,
                        token_type: TokenType::LessEqual,
                    });
                    chars.next();
                }
                _ => {
                    tokens.push(Token {
                        line,
                        token_type: TokenType::Less,
                    });
                }
            },
            '>' => match chars.peek() {
                Some('=') => {
                    tokens.push(Token {
                        line,
                        token_type: TokenType::GreaterEqual,
                    });
                    chars.next();
                }
                _ => {
                    tokens.push(Token {
                        line,
                        token_type: TokenType::Greater,
                    });
                }
            },
            '"' => {
                let mut string = String::new();
                let string_line = line;
                loop {
                    match chars.peek() {
                        None => {
                            errors.push(
                                format!("\" at line {line} doesn't have a closing \".").into(),
                            );
                            break;
                        }
                        Some(&ch) => {
                            chars.next();
                            match ch {
                                '"' => {
                                    tokens.push(Token {
                                        line: string_line,
                                        token_type: TokenType::Literal(Value::String(string)),
                                    });
                                    break;
                                }
                                '\n' => {
                                    line += 1;
                                    string.push('\n');
                                }
                                ch => {
                                    string.push(ch);
                                }
                            }
                        }
                    }
                }
            }
            char if char.is_ascii_digit() => {
                let mut number = char.to_string();

                // Read the part of the number before floating point.
                loop {
                    let Some(char) = chars.peek() else { break };
                    if char.is_ascii_digit() {
                        number.push(*char);
                        chars.next();
                    } else {
                        break;
                    }
                }

                // Check for floating point and if it has one, add the section after the floating point.
                if let Some(char) = chars.peek() {
                    if *char == '.' {
                        number.push('.');
                        chars.next();
                        loop {
                            let Some(char) = chars.peek() else { break };
                            if char.is_ascii_digit() {
                                number.push(*char);
                                chars.next();
                            } else {
                                break;
                            }
                        }
                    }
                }

                // Parse the number.
                tokens.push(Token {
                    line,
                    token_type: TokenType::Literal(Value::Number(number.parse().unwrap())),
                });
            }
            char if char.is_ascii_alphabetic() || char == '_' => {
                let mut word = char.to_string();
                loop {
                    let Some(char) = chars.peek() else { break };
                    if char.is_ascii_alphanumeric() || *char == '_' {
                        word.push(*char);
                        chars.next();
                    } else {
                        break;
                    }
                }

                match word.as_str() {
                    "true" => {
                        tokens.push(Token {
                            line,
                            token_type: TokenType::Literal(Value::Boolean(true)),
                        });
                    }
                    "false" => {
                        tokens.push(Token {
                            line,
                            token_type: TokenType::Literal(Value::Boolean(false)),
                        });
                    }
                    "let" => tokens.push(Token {
                        line,
                        token_type: TokenType::Let,
                    }),
                    "if" => tokens.push(Token {
                        line,
                        token_type: TokenType::If,
                    }),
                    "else" => tokens.push(Token {
                        line,
                        token_type: TokenType::Else,
                    }),
                    "while" => tokens.push(Token {
                        line,
                        token_type: TokenType::While,
                    }),
                    "number" => tokens.push(Token {
                        line,
                        token_type: TokenType::Number,
                    }),
                    "string" => tokens.push(Token {
                        line,
                        token_type: TokenType::String,
                    }),
                    "bool" => tokens.push(Token {
                        line,
                        token_type: TokenType::Bool,
                    }),
                    _ => tokens.push(Token {
                        line,
                        token_type: TokenType::Variable(word),
                    }),
                }
            }
            _ => errors.push(format!("Invalid token {ch} at line {line}.").into()),
        }
    }

    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors)
    }
}
