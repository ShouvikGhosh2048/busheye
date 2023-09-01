use std::{error::Error, fmt::Display};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Number,
    String,
    Boolean,
    Tuple(Vec<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Tuple(Vec<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(number) => write!(f, "{number}"),
            Value::String(string) => write!(f, "{string}"),
            Value::Boolean(boolean) => write!(f, "{boolean}"),
            Value::Tuple(values) => {
                write!(f, "(")?;
                let mut values = values.iter();
                if let Some(first_value) = values.next() {
                    write!(f, "{first_value}")?;
                    for value in values {
                        write!(f, ", {value}")?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

impl Value {
    pub fn value_type(&self) -> Type {
        match self {
            Value::Number(_) => Type::Number,
            Value::String(_) => Type::String,
            Value::Boolean(_) => Type::Boolean,
            Value::Tuple(values) => {
                Type::Tuple(values.iter().map(|value| value.value_type()).collect())
            }
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
    Comma,
    Dot,
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
            ',' => tokens.push(Token {
                line,
                token_type: TokenType::Comma,
            }),
            '.' => tokens.push(Token {
                line,
                token_type: TokenType::Dot,
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
                // Note: If we have a string "a.2.2", we want to parse it as [variable a, dot, 2, dot 2],
                // and not as [variable a, dot, 2.2].
                // So we don't add a dot if the last token in tokens is a dot.
                if let Some(char) = chars.peek() {
                    if *char == '.'
                        && tokens.last().map(|token| &token.token_type) != Some(&TokenType::Dot)
                    {
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
