use std::{collections::HashMap, error::Error, fmt::Debug};

use crate::tokenizer::{BaseType, Token, TokenType, Value};

pub enum UnaryOperation {
    Minus,
    Not,
}

impl UnaryOperation {
    fn value_type(&self, expression_type: BaseType) -> Result<BaseType, Box<dyn Error>> {
        match self {
            UnaryOperation::Minus => match expression_type {
                BaseType::Number => Ok(BaseType::Number),
                expression_type => Err(format!("Can't apply minus on {expression_type:?}.").into()),
            },
            UnaryOperation::Not => match expression_type {
                BaseType::Boolean => Ok(BaseType::Boolean),
                expression_type => {
                    Err(format!("Can't apply not operator on {expression_type:?}.").into())
                }
            },
        }
    }
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

impl BinaryOperation {
    pub fn value_type(
        &self,
        left_type: BaseType,
        right_type: BaseType,
    ) -> Result<BaseType, Box<dyn Error>> {
        match self {
            BinaryOperation::Add => match (left_type, right_type) {
                (BaseType::Number, BaseType::Number) => Ok(BaseType::Number),
                (BaseType::String, BaseType::String) => Ok(BaseType::String),
                _ => Err(format!("Can't add {left_type:?} and {right_type:?}.").into()),
            },
            BinaryOperation::Subtract => match (left_type, right_type) {
                (BaseType::Number, BaseType::Number) => Ok(BaseType::Number),
                _ => Err(format!("Can't subtract {right_type:?} from {left_type:?}.").into()),
            },
            BinaryOperation::Multiply => match (left_type, right_type) {
                (BaseType::Number, BaseType::Number) => Ok(BaseType::Number),
                _ => Err(format!("Can't add {left_type:?} and {right_type:?}.").into()),
            },
            BinaryOperation::Divide => match (left_type, right_type) {
                (BaseType::Number, BaseType::Number) => Ok(BaseType::Number),
                _ => Err(format!("Can't divide {left_type:?} by {right_type:?}.").into()),
            },
            BinaryOperation::Equal => match (left_type, right_type) {
                (BaseType::Number, BaseType::Number)
                | (BaseType::String, BaseType::String)
                | (BaseType::Boolean, BaseType::Boolean) => Ok(BaseType::Boolean),
                _ => Err(format!("Can't compare {left_type:?} with {right_type:?}.").into()),
            },
            BinaryOperation::NotEqual => match (left_type, right_type) {
                (BaseType::Number, BaseType::Number)
                | (BaseType::String, BaseType::String)
                | (BaseType::Boolean, BaseType::Boolean) => Ok(BaseType::Boolean),
                _ => Err(format!("Can't compare {left_type:?} with {right_type:?}.").into()),
            },
            BinaryOperation::Less => match (left_type, right_type) {
                (BaseType::Number, BaseType::Number) | (BaseType::String, BaseType::String) => {
                    Ok(BaseType::Boolean)
                }
                _ => Err(format!("Can't compare {left_type:?} with {right_type:?}.").into()),
            },
            BinaryOperation::LessEqual => match (left_type, right_type) {
                (BaseType::Number, BaseType::Number) | (BaseType::String, BaseType::String) => {
                    Ok(BaseType::Boolean)
                }
                _ => Err(format!("Can't compare {left_type:?} with {right_type:?}.").into()),
            },
            BinaryOperation::Greater => match (left_type, right_type) {
                (BaseType::Number, BaseType::Number) | (BaseType::String, BaseType::String) => {
                    Ok(BaseType::Boolean)
                }
                _ => Err(format!("Can't compare {left_type:?} with {right_type:?}.").into()),
            },
            BinaryOperation::GreaterEqual => match (left_type, right_type) {
                (BaseType::Number, BaseType::Number) | (BaseType::String, BaseType::String) => {
                    Ok(BaseType::Boolean)
                }
                _ => Err(format!("Can't compare {left_type:?} with {right_type:?}.").into()),
            },
            BinaryOperation::Or => match (left_type, right_type) {
                (BaseType::Boolean, BaseType::Boolean) => Ok(BaseType::Boolean),
                _ => Err(
                    format!("Can't apply || operator on {left_type:?} by {right_type:?}.").into(),
                ),
            },
            BinaryOperation::And => match (left_type, right_type) {
                (BaseType::Boolean, BaseType::Boolean) => Ok(BaseType::Boolean),
                _ => Err(
                    format!("Can't apply && operator on {left_type:?} by {right_type:?}.").into(),
                ),
            },
            BinaryOperation::Assignment => match (left_type, right_type) {
                (BaseType::Number, BaseType::Number) => Ok(BaseType::Number),
                (BaseType::String, BaseType::String) => Ok(BaseType::String),
                (BaseType::Boolean, BaseType::Boolean) => Ok(BaseType::Boolean),
                _ => Err(format!("Can't assign {right_type:?} to {left_type:?}.").into()),
            },
        }
    }
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
}

pub struct Expression {
    pub value_type: BaseType,
    pub expression_type: ExpressionType,
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
            },
            ExpressionType::Variable(variable) => {
                write!(f, "{variable}")
            }
            ExpressionType::Grouping(expression) => {
                write!(f, "({expression:?})")
            }
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    VariableDeclaration {
        variable: String,
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

pub fn get_type(variable: &String, stack: &[HashMap<String, BaseType>]) -> Option<BaseType> {
    for map in stack.iter().rev() {
        if let Some(base_type) = map.get(variable) {
            return Some(*base_type);
        }
    }

    None
}

pub fn set_type(variable: &str, base_type: BaseType, stack: &mut [HashMap<String, BaseType>]) {
    if let Some(map) = stack.iter_mut().next_back() {
        map.insert(variable.to_owned(), base_type);
    }
}

pub fn parse(
    tokens: &Vec<Token>,
    global_variables: &HashMap<String, BaseType>,
) -> Result<Vec<Statement>, Vec<Box<dyn Error>>> {
    let mut current_token = 0;
    let mut errors = vec![];
    let mut variables = vec![global_variables.clone()];
    let mut statements = vec![];

    while current_token < tokens.len() {
        let Some(statement) = parse_statement(tokens, &mut current_token, &mut errors, &mut variables) else {
            return Err(errors);
        };
        statements.push(statement);
    }

    if errors.is_empty() {
        Ok(statements)
    } else {
        Err(errors)
    }
}

/// Parses a block statement.
pub fn parse_block_statement(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, BaseType>>,
) -> Option<Statement> {
    if tokens.get(*current_token).map(|token| &token.token_type) != Some(&TokenType::LeftBrace) {
        errors.push("Expected left brace.".into());
        return None;
    }
    *current_token += 1;

    variables.push(HashMap::new());
    let mut statements = vec![];
    loop {
        match tokens.get(*current_token).map(|token| &token.token_type) {
            Some(&TokenType::RightBrace) => {
                *current_token += 1;
                variables.pop();
                break;
            }
            None => {
                errors.push("Unmatched left brace.".into());
                variables.pop();
                return None;
            }
            _ => {
                let Some(statement) = parse_statement(tokens, current_token, errors, variables) else {
                    variables.pop();
                    return None;
                };
                statements.push(statement);
            }
        }
    }
    Some(Statement::Block(statements))
}

pub fn parse_statement(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, BaseType>>,
) -> Option<Statement> {
    match tokens[*current_token].token_type {
        TokenType::Let => {
            *current_token += 1;

            let variable = match tokens.get(*current_token).map(|token| &token.token_type) {
                Some(TokenType::Variable(variable)) => variable.clone(),
                _ => {
                    errors.push("Expected variable after let.".into());
                    return None;
                }
            };
            *current_token += 1;

            let variable_type = match tokens.get(*current_token).map(|token| &token.token_type) {
                Some(TokenType::Colon) => {
                    *current_token += 1;
                    let variable_type =
                        match tokens.get(*current_token).map(|token| &token.token_type) {
                            Some(TokenType::Number) => Some(BaseType::Number),
                            Some(TokenType::String) => Some(BaseType::String),
                            Some(TokenType::Bool) => Some(BaseType::Boolean),
                            val => {
                                println!("{val:?}");
                                errors.push("Expected type after :.".into());
                                return None;
                            }
                        };
                    *current_token += 1;
                    variable_type
                }
                _ => None,
            };

            if tokens.get(*current_token).map(|token| &token.token_type) != Some(&TokenType::Equal)
            {
                errors.push("Expected equal symbol in let statement.".into());
                return None;
            };
            *current_token += 1;

            let Some(expression) = parse_expression(tokens, current_token, errors, variables) else {
                // TODO: Instead of immediately returning, panic to the next possible start.
                return None;
            };

            if tokens.get(*current_token).map(|token| &token.token_type)
                != Some(&TokenType::Semicolon)
            {
                errors.push("Expected semicolon after statement.".into());
                return None;
            }
            *current_token += 1;

            if let Some(variable_type) = variable_type {
                if variable_type != expression.value_type {
                    errors.push(
                        format!(
                            "Can't assign {:?} to {variable_type:?}.",
                            expression.value_type
                        )
                        .into(),
                    );
                    return None;
                }
            }

            set_type(&variable, expression.value_type, variables);
            Some(Statement::VariableDeclaration {
                variable,
                value: expression,
            })
        }
        TokenType::LeftBrace => parse_block_statement(tokens, current_token, errors, variables),
        TokenType::If => {
            *current_token += 1;

            let Some(expression) = parse_expression(tokens, current_token, errors, variables) else {
                errors.push("Expected expression after if.".into());
                return None;
            };

            let Some(then_statement) = parse_block_statement(tokens, current_token, errors, variables).map(|statement| statement.into()) else {
                errors.push("Expected statement after expression.".into());
                return None;
            };

            let else_statement = if tokens.get(*current_token).map(|token| &token.token_type)
                == Some(&TokenType::Else)
            {
                *current_token += 1;
                let Some(else_statement) = parse_block_statement(tokens, current_token, errors, variables) else {
                    errors.push("Expected statement after expression.".into());
                    return None;
                };
                Some(else_statement.into())
            } else {
                None
            };

            if expression.value_type != BaseType::Boolean {
                errors.push("Expected boolean expression.".into());
                return None;
            }

            Some(Statement::If {
                expression,
                then_statement,
                else_statement,
            })
        }
        TokenType::While => {
            *current_token += 1;

            let Some(expression) = parse_expression(tokens, current_token, errors, variables) else {
                errors.push("Expected expression after while.".into());
                return None;
            };

            let Some(statement) = parse_block_statement(tokens, current_token, errors, variables).map(|statement| statement.into()) else {
                errors.push("Expected block statement for while.".into());
                return None;
            };

            if expression.value_type != BaseType::Boolean {
                errors.push("Expected boolean expression in while.".into());
                return None;
            }

            Some(Statement::While {
                expression,
                statement,
            })
        }
        _ => {
            let Some(expression) = parse_expression(tokens, current_token, errors, variables) else {
                // TODO: Instead of immediately returning, panic to the next possible start.
                return None;
            };
            if tokens.get(*current_token).map(|token| &token.token_type)
                != Some(&TokenType::Semicolon)
            {
                errors.push("Expected semicolon after statement.".into());
                return None;
            }
            *current_token += 1;
            Some(Statement::Expression(expression))
        }
    }
}

// The parse functions below try to parse an expression from tokens
// starting from the token at current_token.
// If an error occurs, it's added to the errors vector.

fn parse_expression(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, BaseType>>,
) -> Option<Expression> {
    parse_assignment(tokens, current_token, errors, variables)
}

fn parse_assignment(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, BaseType>>,
) -> Option<Expression> {
    let variable = match (
        tokens.get(*current_token).map(|token| &token.token_type),
        tokens
            .get(*current_token + 1)
            .map(|token| &token.token_type),
    ) {
        (Some(TokenType::Variable(variable)), Some(TokenType::Equal)) => variable.clone(),
        _ => {
            return parse_or(tokens, current_token, errors, variables);
        }
    };
    *current_token += 2;

    let Some(expression) = parse_or(tokens, current_token, errors, variables) else {
        return None;
    };

    let Some(variable_type) = get_type(&variable, variables) else {
        errors.push(format!("No variable called {variable} exists.").into());
        return None;
    };

    let value_type =
        match BinaryOperation::Assignment.value_type(variable_type, expression.value_type) {
            Ok(value_type) => value_type,
            Err(err) => {
                errors.push(err);
                return None;
            }
        };

    Some(Expression {
        value_type,
        expression_type: ExpressionType::Binary {
            operation: BinaryOperation::Assignment,
            left_expression: Expression {
                value_type,
                expression_type: ExpressionType::Variable(variable),
            }
            .into(),
            right_expression: expression.into(),
        },
    })
}

fn parse_or(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, BaseType>>,
) -> Option<Expression> {
    let Some(mut expression) = parse_and(tokens, current_token, errors, variables) else {
        return None;
    };

    loop {
        let operation = match tokens.get(*current_token).map(|token| &token.token_type) {
            Some(TokenType::DoubleOr) => BinaryOperation::Or,
            _ => return Some(expression),
        };

        *current_token += 1;

        let Some(right_expression) = parse_and(tokens, current_token, errors, variables) else {
            return None;
        };

        let value_type =
            match operation.value_type(expression.value_type, right_expression.value_type) {
                Ok(value_type) => value_type,
                Err(err) => {
                    errors.push(err);
                    return None;
                }
            };

        expression = Expression {
            value_type,
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
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, BaseType>>,
) -> Option<Expression> {
    let Some(mut expression) = parse_comparison(tokens, current_token, errors, variables) else {
        return None;
    };

    loop {
        let operation = match tokens.get(*current_token).map(|token| &token.token_type) {
            Some(TokenType::DoubleAnd) => BinaryOperation::And,
            _ => return Some(expression),
        };

        *current_token += 1;

        let Some(right_expression) = parse_comparison(tokens, current_token, errors, variables) else {
            return None;
        };

        let value_type =
            match operation.value_type(expression.value_type, right_expression.value_type) {
                Ok(value_type) => value_type,
                Err(err) => {
                    errors.push(err);
                    return None;
                }
            };

        expression = Expression {
            value_type,
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
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, BaseType>>,
) -> Option<Expression> {
    let Some(mut expression) = parse_term(tokens, current_token, errors, variables) else {
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

        let Some(right_expression) = parse_term(tokens, current_token, errors, variables) else {
            return None;
        };

        let value_type =
            match operation.value_type(expression.value_type, right_expression.value_type) {
                Ok(value_type) => value_type,
                Err(err) => {
                    errors.push(err);
                    return None;
                }
            };

        expression = Expression {
            value_type,
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
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, BaseType>>,
) -> Option<Expression> {
    let Some(mut expression) = parse_factor(tokens, current_token, errors, variables) else {
        return None;
    };

    loop {
        let operation = match tokens.get(*current_token).map(|token| &token.token_type) {
            Some(TokenType::Plus) => BinaryOperation::Add,
            Some(TokenType::Minus) => BinaryOperation::Subtract,
            _ => return Some(expression),
        };

        *current_token += 1;

        let Some(right_expression) = parse_factor(tokens, current_token, errors, variables) else {
            return None;
        };

        let value_type =
            match operation.value_type(expression.value_type, right_expression.value_type) {
                Ok(value_type) => value_type,
                Err(err) => {
                    errors.push(err);
                    return None;
                }
            };

        expression = Expression {
            value_type,
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
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, BaseType>>,
) -> Option<Expression> {
    let Some(mut expression) = parse_unary(tokens, current_token, errors, variables) else {
        return None;
    };

    loop {
        let operation = match tokens.get(*current_token).map(|token| &token.token_type) {
            Some(TokenType::Star) => BinaryOperation::Multiply,
            Some(TokenType::Slash) => BinaryOperation::Divide,
            _ => return Some(expression),
        };

        *current_token += 1;

        let Some(right_expression) = parse_unary(tokens, current_token, errors, variables) else {
            return None;
        };

        let value_type =
            match operation.value_type(expression.value_type, right_expression.value_type) {
                Ok(value_type) => value_type,
                Err(err) => {
                    errors.push(err);
                    return None;
                }
            };

        expression = Expression {
            value_type,
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
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, BaseType>>,
) -> Option<Expression> {
    let operation = match tokens.get(*current_token).map(|token| &token.token_type) {
        Some(TokenType::Minus) => UnaryOperation::Minus,
        Some(TokenType::Exclamation) => UnaryOperation::Not,
        _ => return parse_primary(tokens, current_token, errors, variables),
    };

    *current_token += 1;

    let Some(expression) = parse_unary(tokens, current_token, errors, variables) else {
        return None;
    };

    let value_type = match operation.value_type(expression.value_type) {
        Ok(value_type) => value_type,
        Err(err) => {
            errors.push(err);
            return None;
        }
    };

    Some(Expression {
        value_type,
        expression_type: ExpressionType::Unary {
            operation,
            expression: expression.into(),
        },
    })
}

fn parse_primary(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, BaseType>>,
) -> Option<Expression> {
    match tokens.get(*current_token) {
        None => {
            errors.push("No tokens available to parse".into());
            None
        }
        Some(token) => match &token.token_type {
            TokenType::Literal(value) => {
                *current_token += 1;
                let value_type = value.base_type();
                Some(Expression {
                    value_type,
                    expression_type: ExpressionType::Literal(value.clone()),
                })
            }
            TokenType::Variable(variable) => {
                *current_token += 1;
                let Some(value_type) = get_type(variable, variables) else {
                    errors.push(format!("No variable called {variable} exists.").into());
                    return None;
                };
                Some(Expression {
                    value_type,
                    expression_type: ExpressionType::Variable(variable.clone()),
                })
            }
            TokenType::LeftParenthesis => {
                *current_token += 1;
                let Some(expression) = parse_expression(tokens, current_token, errors, variables) else { return None };
                if tokens.get(*current_token).map(|token| &token.token_type)
                    == Some(&TokenType::RightParenthesis)
                {
                    *current_token += 1;
                    Some(Expression {
                        value_type: expression.value_type,
                        expression_type: ExpressionType::Grouping(expression.into()),
                    })
                } else {
                    errors.push(
                        format!(
                            "No matching paranthesis for the left parenthesis on line {}.",
                            token.line
                        )
                        .into(),
                    );
                    None
                }
            }
            _ => {
                errors.push("No tokens available to parse".into());
                None
            }
        },
    }
}
