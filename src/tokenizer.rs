use std::{
    error::Error,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::parser::Statement;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Void,
    Any,
    Number,
    String,
    Boolean,
    Tuple(Vec<Type>),
    Function {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
}

#[derive(Clone)]
pub enum FunctionBody {
    Statement(Box<Statement>),
    RustClosure {
        id: usize,
        closure: Rc<dyn Fn(Vec<Value>) -> Value>,
    },
}

impl Debug for FunctionBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionBody::Statement(statement) => write!(f, "{statement:?}"),
            FunctionBody::RustClosure { .. } => write!(f, "Rust closure"),
        }
    }
}

impl PartialEq for FunctionBody {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (FunctionBody::Statement(statement1), FunctionBody::Statement(statement2)) => {
                statement1 == statement2
            }
            (
                FunctionBody::RustClosure { id: id1, .. },
                FunctionBody::RustClosure { id: id2, .. },
            ) => id1 == id2,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Void,
    Number(f64),
    String(String),
    Boolean(bool),
    Tuple(Vec<Value>),
    Function {
        parameters: Vec<(String, Option<usize>, Type)>, // Option<usize> is the shadow_id of the parameter.
        return_type: Type,
        body: FunctionBody,
        parent_environment: usize, // ID of the environment in which the function was defined.
    },
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Void => write!(f, "void"),
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
            Value::Function { .. } => {
                write!(f, "function") // TODO: Improve this
            }
        }
    }
}

impl Value {
    pub fn value_type(&self) -> Type {
        match self {
            Value::Void => Type::Void,
            Value::Number(_) => Type::Number,
            Value::String(_) => Type::String,
            Value::Boolean(_) => Type::Boolean,
            Value::Tuple(values) => {
                Type::Tuple(values.iter().map(|value| value.value_type()).collect())
            }
            Value::Function {
                parameters,
                return_type,
                ..
            } => Type::Function {
                parameters: parameters
                    .iter()
                    .map(|(_, _, parameter_type)| parameter_type.clone())
                    .collect(),
                return_type: return_type.clone().into(),
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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
    Arrow,
    Let,
    If,
    Else,
    While,
    Fn,
    Return,
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
    pub lines: (usize, usize),
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
                lines: (line, line),
                token_type: TokenType::Plus,
            }),
            '-' => match chars.peek() {
                Some('>') => {
                    tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::Arrow,
                    });
                    chars.next();
                }
                _ => {
                    tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::Minus,
                    });
                }
            },
            '*' => tokens.push(Token {
                lines: (line, line),
                token_type: TokenType::Star,
            }),
            '/' => tokens.push(Token {
                lines: (line, line),
                token_type: TokenType::Slash,
            }),
            '(' => tokens.push(Token {
                lines: (line, line),
                token_type: TokenType::LeftParenthesis,
            }),
            ')' => tokens.push(Token {
                lines: (line, line),
                token_type: TokenType::RightParenthesis,
            }),
            '{' => tokens.push(Token {
                lines: (line, line),
                token_type: TokenType::LeftBrace,
            }),
            '}' => tokens.push(Token {
                lines: (line, line),
                token_type: TokenType::RightBrace,
            }),
            ':' => tokens.push(Token {
                lines: (line, line),
                token_type: TokenType::Colon,
            }),
            ';' => tokens.push(Token {
                lines: (line, line),
                token_type: TokenType::Semicolon,
            }),
            ',' => tokens.push(Token {
                lines: (line, line),
                token_type: TokenType::Comma,
            }),
            '.' => tokens.push(Token {
                lines: (line, line),
                token_type: TokenType::Dot,
            }),
            '&' => match chars.peek() {
                Some('&') => {
                    tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::DoubleAnd,
                    });
                    chars.next();
                }
                _ => errors.push(
                    format!(
                        "Tokenizer error: Invalid token {ch} at line {line}. (Did you mean && ?)"
                    )
                    .into(),
                ),
            },
            '|' => match chars.peek() {
                Some('|') => {
                    tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::DoubleOr,
                    });
                    chars.next();
                }
                _ => errors.push(
                    format!(
                        "Tokenizer error: Invalid token {ch} at line {line}. (Did you mean || ?)"
                    )
                    .into(),
                ),
            },
            '=' => match chars.peek() {
                Some('=') => {
                    tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::DoubleEqual,
                    });
                    chars.next();
                }
                _ => {
                    tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::Equal,
                    });
                }
            },
            '!' => match chars.peek() {
                Some('=') => {
                    tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::ExclamationEqual,
                    });
                    chars.next();
                }
                _ => {
                    tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::Exclamation,
                    });
                }
            },
            '<' => match chars.peek() {
                Some('=') => {
                    tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::LessEqual,
                    });
                    chars.next();
                }
                _ => {
                    tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::Less,
                    });
                }
            },
            '>' => match chars.peek() {
                Some('=') => {
                    tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::GreaterEqual,
                    });
                    chars.next();
                }
                _ => {
                    tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::Greater,
                    });
                }
            },
            '"' => {
                let mut string = String::new();
                let start_line = line;
                loop {
                    match chars.peek() {
                        None => {
                            errors.push(
                                format!(
                                    "Tokenizer error: \" at line {line} doesn't have a closing \"."
                                )
                                .into(),
                            );
                            break;
                        }
                        Some(&ch) => {
                            chars.next();
                            match ch {
                                '"' => {
                                    tokens.push(Token {
                                        lines: (start_line, line),
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
                    lines: (line, line),
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
                            lines: (line, line),
                            token_type: TokenType::Literal(Value::Boolean(true)),
                        });
                    }
                    "false" => {
                        tokens.push(Token {
                            lines: (line, line),
                            token_type: TokenType::Literal(Value::Boolean(false)),
                        });
                    }
                    "let" => tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::Let,
                    }),
                    "if" => tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::If,
                    }),
                    "else" => tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::Else,
                    }),
                    "while" => tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::While,
                    }),
                    "number" => tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::Number,
                    }),
                    "string" => tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::String,
                    }),
                    "bool" => tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::Bool,
                    }),
                    "fn" => tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::Fn,
                    }),
                    "return" => tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::Return,
                    }),
                    _ => tokens.push(Token {
                        lines: (line, line),
                        token_type: TokenType::Variable(word),
                    }),
                }
            }
            _ => errors.push(format!("Tokenizer error: Invalid token {ch} at line {line}.").into()),
        }
    }

    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors)
    }
}
