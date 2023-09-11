use std::fmt::{Debug, Display};

use crate::tokenizer::{Token, TokenType, Type, Value};

pub enum UnaryOperation {
    Minus,
    Not,
}

impl Debug for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use UnaryOperation::*;

        let operator = match self {
            Minus => "-",
            Not => "!",
        };

        write!(f, "{operator}")
    }
}

#[derive(PartialEq)]
pub enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    Assignment,
}

impl Debug for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BinaryOperation::*;

        let operator = match self {
            Add => "+",
            Subtract => "-",
            Multiply => "*",
            Divide => "/",
            Equal => "==",
            NotEqual => "!=",
            Less => "<",
            LessEqual => "<=",
            Greater => ">",
            GreaterEqual => ">=",
            Or => "||",
            And => "&&",
            Assignment => "=",
        };

        write!(f, "{operator}")
    }
}

pub enum ExpressionType {
    Unary {
        operation: UnaryOperation,
        expression: Box<Expression>,
    },
    Binary {
        operation: BinaryOperation,
        left_expression: Box<Expression>,
        right_expression: Box<Expression>,
    },
    Literal(Value),
    Variable(String),
    Grouping(Box<Expression>),
    Tuple(Vec<Expression>),
    TupleAccess {
        expression: Box<Expression>,
        index: usize,
    },
}

pub struct Expression {
    pub expression_type: ExpressionType,
    pub lines: (usize, usize),
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.expression_type {
            ExpressionType::Unary {
                operation,
                expression,
            } => {
                write!(f, "{operation:?} ({expression:?})")
            }
            ExpressionType::Binary {
                operation,
                left_expression,
                right_expression,
            } => {
                write!(
                    f,
                    "{operation:?} ({left_expression:?}) ({right_expression:?})"
                )
            }
            ExpressionType::Literal(value) => match value {
                Value::Number(value) => write!(f, "{value}"),
                Value::String(value) => write!(f, "{value}"),
                Value::Boolean(value) => write!(f, "{value}"),
                Value::Tuple(_) => {
                    unreachable!("Tuple's are created from tuple expressions.")
                }
            },
            ExpressionType::Variable(variable) => {
                write!(f, "{variable}")
            }
            ExpressionType::Grouping(expression) => {
                write!(f, "({expression:?})")
            }
            ExpressionType::Tuple(expressions) => {
                write!(f, "(")?;
                let mut expressions = expressions.iter();
                if let Some(first_expression) = expressions.next() {
                    write!(f, "{first_expression:?},")?;
                    if let Some(second_expression) = expressions.next() {
                        write!(f, "{second_expression:?}")?;
                        for expression in expressions {
                            write!(f, ",{expression:?}")?;
                        }
                    }
                }
                write!(f, ")")?;
                Ok(())
            }
            ExpressionType::TupleAccess { expression, index } => {
                write!(f, ". {expression:?} {index}")
            }
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.expression_type {
            ExpressionType::Unary {
                operation,
                expression,
            } => {
                write!(f, "{operation:?}{expression}")
            }
            ExpressionType::Binary {
                operation,
                left_expression,
                right_expression,
            } => {
                write!(f, "{left_expression} {operation:?} {right_expression}")
            }
            ExpressionType::TupleAccess { expression, index } => {
                write!(f, "{expression}.{index}")
            }
            _ => {
                write!(f, "{self:?}")
            }
        }
    }
}

#[derive(Debug)]
pub enum StatementType {
    VariableDeclaration {
        variable: String,
        variable_type: Option<Type>,
        value: Expression,
    },
    Expression(Expression),
    Block(Vec<Statement>),
    If {
        expression: Expression,
        then_statement: Box<Statement>,
        else_statement: Option<Box<Statement>>,
    },
    While {
        expression: Expression,
        statement: Box<Statement>,
    },
}

#[derive(Debug)]
pub struct Statement {
    pub statement: StatementType,
    pub lines: (usize, usize),
}

#[derive(Debug)]
pub struct CompilerError {
    pub lines: (usize, usize),
    pub error: String,
}

pub fn parse(tokens: &Vec<Token>) -> Result<Vec<Statement>, Vec<CompilerError>> {
    let mut current_token = 0;
    let mut errors = vec![];
    let mut statements = vec![];

    while current_token < tokens.len() {
        let Some(statement) = parse_statement(tokens, &mut current_token, &mut errors) else {
            continue;
        };
        statements.push(statement);
    }

    if errors.is_empty() {
        Ok(statements)
    } else {
        Err(errors)
    }
}

fn panic_forward(tokens: &Vec<Token>, current_token: &mut usize) {
    while *current_token < tokens.len() {
        if matches!(
            tokens[*current_token].token_type,
            TokenType::If
                | TokenType::While
                | TokenType::Let
                | TokenType::LeftBrace
                | TokenType::RightBrace
        ) {
            break;
        }

        *current_token += 1;
    }
}

/// Parses a block statement.
pub fn parse_block_statement(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<CompilerError>,
) -> Option<Statement> {
    let start_line = match tokens
        .get(*current_token)
        .map(|token| (&token.token_type, token))
    {
        Some((TokenType::LeftBrace, token)) => token.lines.0,
        _ => {
            panic_forward(tokens, current_token);
            return None;
        }
    };
    *current_token += 1;

    let mut statements = vec![];
    let mut statement_error_occured = false;
    let mut end_line = start_line;
    loop {
        match tokens
            .get(*current_token)
            .map(|token| (&token.token_type, token))
        {
            Some((&TokenType::RightBrace, token)) => {
                end_line = token.lines.1;
                *current_token += 1;
                break;
            }
            None => {
                errors.push(CompilerError {
                    lines: (start_line, end_line),
                    error: "Unmatched left brace.".into(),
                });
                return None;
            }
            _ => {
                let Some(statement) = parse_statement(tokens, current_token, errors) else {
                    statement_error_occured = true;
                    // If panic_forward stopped at a right brace, we break out of the block.
                    if tokens[*current_token - 1].token_type == TokenType::RightBrace {
                        return None;
                    }
                    continue;
                };
                end_line = statement.lines.1;
                statements.push(statement);
            }
        }
    }

    if statement_error_occured {
        return None;
    }

    Some(Statement {
        lines: (start_line, end_line),
        statement: StatementType::Block(statements),
    })
}

pub fn parse_statement(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<CompilerError>,
) -> Option<Statement> {
    match tokens
        .get(*current_token)
        .map(|token| (&token.token_type, token))
    {
        Some((&TokenType::Let, token)) => {
            let line_start = token.lines.0;
            *current_token += 1;

            let (variable, variable_end) = match tokens
                .get(*current_token)
                .map(|token| (&token.token_type, token))
            {
                Some((TokenType::Variable(variable), token)) => (variable.clone(), token.lines.1),
                _ => {
                    errors.push(CompilerError {
                        lines: (line_start, token.lines.1),
                        error: "Expected variable after let.".into(),
                    });
                    panic_forward(tokens, current_token);
                    return None;
                }
            };
            *current_token += 1;

            let variable_type = match tokens.get(*current_token).map(|token| &token.token_type) {
                Some(TokenType::Colon) => {
                    *current_token += 1;
                    let Some(variable_type) = parse_type(tokens, current_token, errors) else {
                        panic_forward(tokens, current_token);
                        return None;
                    };
                    Some(variable_type)
                }
                _ => None,
            };

            let Some(equal_token) = tokens.get(*current_token) else {
                errors.push(CompilerError { lines: (line_start, variable_end), error: "Expected equal symbol in let statement.".into() });
                panic_forward(tokens, current_token);
                return None;
            };
            if equal_token.token_type != TokenType::Equal {
                errors.push(CompilerError {
                    lines: (line_start, equal_token.lines.1),
                    error: "Expected equal symbol in let statement.".into(),
                });
                panic_forward(tokens, current_token);
                return None;
            };
            *current_token += 1;

            let Some(expression) = parse_expression(tokens, current_token, errors) else {
                panic_forward(tokens, current_token);
                return None;
            };

            let semicolon_line = if let Some((&TokenType::Semicolon, token)) = tokens
                .get(*current_token)
                .map(|token| (&token.token_type, token))
            {
                token.lines.1
            } else {
                errors.push(CompilerError {
                    lines: (line_start, expression.lines.1),
                    error: "Expected semicolon after let statement.".into(),
                });
                panic_forward(tokens, current_token);
                return None;
            };
            *current_token += 1;

            Some(Statement {
                statement: StatementType::VariableDeclaration {
                    variable,
                    variable_type,
                    value: expression,
                },
                lines: (line_start, semicolon_line),
            })
        }
        Some((TokenType::LeftBrace, _)) => parse_block_statement(tokens, current_token, errors),
        Some((TokenType::If, token)) => {
            let if_line = token.lines.0;
            *current_token += 1;

            let Some(expression) = parse_expression(tokens, current_token, errors) else {
                errors.push(CompilerError { lines: (if_line, if_line), error: "Expected condition after if.".into() });
                panic_forward(tokens, current_token);
                return None;
            };

            let Some(then_statement): Option<Box<Statement>> = parse_block_statement(tokens, current_token, errors).map(|statement| statement.into()) else {
                errors.push(CompilerError { lines: (if_line, expression.lines.1), error: "Expected then block after condition.".into() });
                return None;
            };

            let (else_statement, end_line) = if let Some((&TokenType::Else, else_token)) = tokens
                .get(*current_token)
                .map(|token| (&token.token_type, token))
            {
                *current_token += 1;
                let Some(else_statement) = parse_block_statement(tokens, current_token, errors) else {
                    errors.push(CompilerError { lines: (if_line, else_token.lines.1), error: "Expected else block after else.".into() });
                    return None;
                };
                let end_line = else_statement.lines.1;
                (Some(else_statement.into()), end_line)
            } else {
                (None, then_statement.lines.1)
            };

            Some(Statement {
                statement: StatementType::If {
                    expression,
                    then_statement,
                    else_statement,
                },
                lines: (if_line, end_line),
            })
        }
        Some((TokenType::While, token)) => {
            let while_start = token.lines.0;
            *current_token += 1;

            let Some(expression) = parse_expression(tokens, current_token, errors) else {
                errors.push(CompilerError { lines: (while_start, while_start), error: "Expected expression after while.".into() });
                panic_forward(tokens, current_token);
                return None;
            };

            let Some(statement) = parse_block_statement(tokens, current_token, errors).map(|statement| statement.into()) else {
                errors.push(CompilerError { lines: (while_start, expression.lines.1), error: "Expected block statement for while.".into() });
                return None;
            };

            Some(Statement {
                lines: (while_start, expression.lines.1),
                statement: StatementType::While {
                    expression,
                    statement,
                },
            })
        }
        _ => {
            let initial_position = *current_token;
            let Some(expression) = parse_expression(tokens, current_token, errors) else {
                if initial_position == *current_token {
                    // Make sure we keep progressing forward.
                    *current_token += 1;
                }
                panic_forward(tokens, current_token);
                return None;
            };
            let semicolon_line = if let Some((&TokenType::Semicolon, token)) = tokens
                .get(*current_token)
                .map(|token| (&token.token_type, token))
            {
                token.lines.1
            } else {
                errors.push(CompilerError {
                    lines: expression.lines,
                    error: "Expected semicolon at the end of the statement.".into(),
                });
                panic_forward(tokens, current_token);
                return None;
            };
            *current_token += 1;
            Some(Statement {
                lines: (expression.lines.0, semicolon_line),
                statement: StatementType::Expression(expression),
            })
        }
    }
}

fn parse_type(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<CompilerError>,
) -> Option<Type> {
    match tokens
        .get(*current_token)
        .map(|token| (&token.token_type, token))
    {
        Some((TokenType::Number, _)) => {
            *current_token += 1;
            Some(Type::Number)
        }
        Some((TokenType::String, _)) => {
            *current_token += 1;
            Some(Type::String)
        }
        Some((TokenType::Bool, _)) => {
            *current_token += 1;
            Some(Type::Boolean)
        }
        Some((TokenType::LeftParenthesis, token)) => {
            let start_line = token.lines.0;
            *current_token += 1;

            let mut types = vec![];
            loop {
                let tuple_type = parse_type(tokens, current_token, errors)?;
                types.push(tuple_type);

                match tokens
                    .get(*current_token)
                    .map(|token| (&token.token_type, token))
                {
                    Some((TokenType::RightParenthesis, token)) => {
                        *current_token += 1;
                        if types.len() == 1 {
                            errors.push(CompilerError {
                                lines: (start_line, token.lines.1),
                                error: "Tuple type must have at least two elements.".into(),
                            });
                            return None;
                        }
                        return Some(Type::Tuple(types));
                    }
                    Some((TokenType::Comma, _)) => {
                        *current_token += 1;
                    }
                    _ => {
                        errors.push(CompilerError {
                            lines: (start_line, token.lines.1),
                            error: "Invalid type.".into(),
                        });
                        return None;
                    }
                }
            }
        }
        Some((_, token)) => {
            errors.push(CompilerError {
                lines: (token.lines.1, token.lines.1),
                error: "Invalid type.".into(),
            });
            None
        }
        _ => {
            // TODO: Fix the lines shown.
            errors.push(CompilerError {
                lines: (1, 1),
                error: "EOF while parsing type".into(),
            });
            None
        }
    }
}

// The parse functions below try to parse an expression from tokens
// starting from the token at current_token.
// If an error occurs, it's added to the errors vector.

fn parse_expression(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<CompilerError>,
) -> Option<Expression> {
    parse_assignment(tokens, current_token, errors)
}

fn parse_assignment(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<CompilerError>,
) -> Option<Expression> {
    let Some(left_expression) = parse_or(tokens, current_token, errors) else {
        return None;
    };

    if tokens.get(*current_token).map(|token| &token.token_type) != Some(&TokenType::Equal) {
        return Some(left_expression);
    }
    *current_token += 1;

    let Some(right_expression) = parse_assignment(tokens, current_token, errors) else {
        return None;
    };

    Some(Expression {
        lines: (left_expression.lines.0, right_expression.lines.1),
        expression_type: ExpressionType::Binary {
            operation: BinaryOperation::Assignment,
            left_expression: left_expression.into(),
            right_expression: right_expression.into(),
        },
    })
}

fn parse_or(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<CompilerError>,
) -> Option<Expression> {
    let Some(mut expression) = parse_and(tokens, current_token, errors) else {
        return None;
    };

    loop {
        let operation = match tokens.get(*current_token).map(|token| &token.token_type) {
            Some(TokenType::DoubleOr) => BinaryOperation::Or,
            _ => return Some(expression),
        };

        *current_token += 1;

        let Some(right_expression) = parse_and(tokens, current_token, errors) else {
            return None;
        };

        expression = Expression {
            lines: (expression.lines.0, right_expression.lines.1),
            expression_type: ExpressionType::Binary {
                operation,
                left_expression: expression.into(),
                right_expression: right_expression.into(),
            },
        };
    }
}

fn parse_and(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<CompilerError>,
) -> Option<Expression> {
    let Some(mut expression) = parse_comparison(tokens, current_token, errors) else {
        return None;
    };

    loop {
        let operation = match tokens.get(*current_token).map(|token| &token.token_type) {
            Some(TokenType::DoubleAnd) => BinaryOperation::And,
            _ => return Some(expression),
        };

        *current_token += 1;

        let Some(right_expression) = parse_comparison(tokens, current_token, errors) else {
            return None;
        };

        expression = Expression {
            lines: (expression.lines.0, right_expression.lines.1),
            expression_type: ExpressionType::Binary {
                operation,
                left_expression: expression.into(),
                right_expression: right_expression.into(),
            },
        };
    }
}

fn parse_comparison(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<CompilerError>,
) -> Option<Expression> {
    let Some(mut expression) = parse_term(tokens, current_token, errors) else {
        return None;
    };

    loop {
        let operation = match tokens.get(*current_token).map(|token| &token.token_type) {
            Some(TokenType::DoubleEqual) => BinaryOperation::Equal,
            Some(TokenType::ExclamationEqual) => BinaryOperation::NotEqual,
            Some(TokenType::Less) => BinaryOperation::Less,
            Some(TokenType::LessEqual) => BinaryOperation::LessEqual,
            Some(TokenType::Greater) => BinaryOperation::Greater,
            Some(TokenType::GreaterEqual) => BinaryOperation::GreaterEqual,
            _ => return Some(expression),
        };

        *current_token += 1;

        let Some(right_expression) = parse_term(tokens, current_token, errors) else {
            return None;
        };

        expression = Expression {
            lines: (expression.lines.0, right_expression.lines.1),
            expression_type: ExpressionType::Binary {
                operation,
                left_expression: expression.into(),
                right_expression: right_expression.into(),
            },
        };
    }
}

fn parse_term(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<CompilerError>,
) -> Option<Expression> {
    let Some(mut expression) = parse_factor(tokens, current_token, errors) else {
        return None;
    };

    loop {
        let operation = match tokens.get(*current_token).map(|token| &token.token_type) {
            Some(TokenType::Plus) => BinaryOperation::Add,
            Some(TokenType::Minus) => BinaryOperation::Subtract,
            _ => return Some(expression),
        };

        *current_token += 1;

        let Some(right_expression) = parse_factor(tokens, current_token, errors) else {
            return None;
        };

        expression = Expression {
            lines: (expression.lines.0, right_expression.lines.1),
            expression_type: ExpressionType::Binary {
                operation,
                left_expression: expression.into(),
                right_expression: right_expression.into(),
            },
        };
    }
}

fn parse_factor(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<CompilerError>,
) -> Option<Expression> {
    let Some(mut expression) = parse_unary(tokens, current_token, errors) else {
        return None;
    };

    loop {
        let operation = match tokens.get(*current_token).map(|token| &token.token_type) {
            Some(TokenType::Star) => BinaryOperation::Multiply,
            Some(TokenType::Slash) => BinaryOperation::Divide,
            _ => return Some(expression),
        };

        *current_token += 1;

        let Some(right_expression) = parse_unary(tokens, current_token, errors) else {
            return None;
        };

        expression = Expression {
            lines: (expression.lines.0, right_expression.lines.1),
            expression_type: ExpressionType::Binary {
                operation,
                left_expression: expression.into(),
                right_expression: right_expression.into(),
            },
        };
    }
}

fn parse_unary(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<CompilerError>,
) -> Option<Expression> {
    let operation_token = tokens.get(*current_token);
    let (operation, start_line) = match operation_token.map(|token| (&token.token_type, token)) {
        Some((TokenType::Minus, token)) => (UnaryOperation::Minus, token.lines.0),
        Some((TokenType::Exclamation, token)) => (UnaryOperation::Not, token.lines.0),
        _ => return parse_tuple_access(tokens, current_token, errors),
    };

    *current_token += 1;

    let Some(expression) = parse_unary(tokens, current_token, errors) else {
        return None;
    };

    Some(Expression {
        lines: (start_line, expression.lines.1),
        expression_type: ExpressionType::Unary {
            operation,
            expression: expression.into(),
        },
    })
}

fn parse_tuple_access(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<CompilerError>,
) -> Option<Expression> {
    let Some(mut expression) = parse_primary(tokens, current_token, errors) else {
        return None;
    };

    loop {
        let dot_line = if let Some((&TokenType::Dot, token)) = tokens
            .get(*current_token)
            .map(|token| (&token.token_type, token))
        {
            token.lines.1
        } else {
            return Some(expression);
        };

        *current_token += 1;

        *current_token += 1;
        let (index, end_line) = if let Some(token) = tokens.get(*current_token - 1) {
            if let TokenType::Literal(Value::Number(number)) = token.token_type {
                if number.floor() != number {
                    errors.push(CompilerError {
                        lines: (expression.lines.0, token.lines.1),
                        error: "Expected nonnegative integer index.".into(),
                    });
                    return None;
                }

                (number.floor() as usize, token.lines.1) // TODO: Consider overflow
            } else {
                errors.push(CompilerError {
                    lines: (expression.lines.0, token.lines.1),
                    error: "Expected index after dot.".into(),
                });
                return None;
            }
        } else {
            errors.push(CompilerError {
                lines: (expression.lines.0, dot_line),
                error: "Expected index after dot.".into(),
            });
            return None;
        };

        expression = Expression {
            lines: (expression.lines.0, end_line),
            expression_type: ExpressionType::TupleAccess {
                expression: expression.into(),
                index,
            },
        };
    }
}

fn parse_primary(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<CompilerError>,
) -> Option<Expression> {
    match tokens.get(*current_token) {
        None => {
            // TODO: Fix the lines
            errors.push(CompilerError {
                lines: (1, 1),
                error: "No tokens available to parse (EOF)".into(),
            });
            None
        }
        Some(token) => match &token.token_type {
            TokenType::Literal(value) => {
                *current_token += 1;
                Some(Expression {
                    expression_type: ExpressionType::Literal(value.clone()),
                    lines: token.lines,
                })
            }
            TokenType::Variable(variable) => {
                *current_token += 1;
                Some(Expression {
                    expression_type: ExpressionType::Variable(variable.clone()),
                    lines: token.lines,
                })
            }
            TokenType::LeftParenthesis => {
                let start_line = token.lines.0;
                *current_token += 1;

                let Some(expression) = parse_expression(tokens, current_token, errors) else { return None };

                if let Some(token) = tokens.get(*current_token) {
                    if token.token_type == TokenType::RightParenthesis {
                        *current_token += 1;
                        return Some(Expression {
                            expression_type: ExpressionType::Grouping(expression.into()),
                            lines: (start_line, token.lines.1),
                        });
                    }
                }

                let mut expressions = vec![expression];
                loop {
                    if tokens.get(*current_token).map(|token| &token.token_type)
                        != Some(&TokenType::Comma)
                    {
                        errors.push(CompilerError {
                            lines: (
                                expressions.first().unwrap().lines.0,
                                expressions.last().unwrap().lines.1,
                            ),
                            error: "Expected , in tuple.".into(),
                        });
                        return None;
                    }
                    *current_token += 1;

                    let Some(expression) = parse_expression(tokens, current_token, errors) else { return None };
                    let expression_end = expression.lines.1;
                    expressions.push(expression);

                    if tokens.get(*current_token).map(|token| &token.token_type)
                        == Some(&TokenType::RightParenthesis)
                    {
                        *current_token += 1;
                        return Some(Expression {
                            expression_type: ExpressionType::Tuple(expressions),
                            lines: (start_line, expression_end),
                        });
                    }
                }
            }
            _ => {
                errors.push(CompilerError {
                    lines: token.lines,
                    error: format!(
                        "Expected primary expression, got {:?} instead.",
                        token.token_type
                    ),
                });
                None
            }
        },
    }
}
