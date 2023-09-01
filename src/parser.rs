use std::{collections::HashMap, error::Error, fmt::Debug};

use crate::tokenizer::{Token, TokenType, Type, Value};

pub enum UnaryOperation {
    Minus,
    Not,
}

impl UnaryOperation {
    fn value_type(&self, expression_type: &Type) -> Result<Type, Box<dyn Error>> {
        match self {
            UnaryOperation::Minus => match expression_type {
                Type::Number => Ok(Type::Number),
                expression_type => Err(format!("Can't apply minus on {expression_type:?}.").into()),
            },
            UnaryOperation::Not => match expression_type {
                Type::Boolean => Ok(Type::Boolean),
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
    pub fn value_type(&self, left_type: &Type, right_type: &Type) -> Result<Type, Box<dyn Error>> {
        match self {
            BinaryOperation::Add => match (left_type, right_type) {
                (Type::Number, Type::Number) => Ok(Type::Number),
                (Type::String, Type::String) => Ok(Type::String),
                _ => Err(format!("Can't add {left_type:?} and {right_type:?}.").into()),
            },
            BinaryOperation::Subtract => match (left_type, right_type) {
                (Type::Number, Type::Number) => Ok(Type::Number),
                _ => Err(format!("Can't subtract {right_type:?} from {left_type:?}.").into()),
            },
            BinaryOperation::Multiply => match (left_type, right_type) {
                (Type::Number, Type::Number) => Ok(Type::Number),
                _ => Err(format!("Can't add {left_type:?} and {right_type:?}.").into()),
            },
            BinaryOperation::Divide => match (left_type, right_type) {
                (Type::Number, Type::Number) => Ok(Type::Number),
                _ => Err(format!("Can't divide {left_type:?} by {right_type:?}.").into()),
            },
            BinaryOperation::Equal => match (left_type, right_type) {
                (Type::Number, Type::Number)
                | (Type::String, Type::String)
                | (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
                _ => Err(format!("Can't compare {left_type:?} with {right_type:?}.").into()),
            },
            BinaryOperation::NotEqual => match (left_type, right_type) {
                (Type::Number, Type::Number)
                | (Type::String, Type::String)
                | (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
                _ => Err(format!("Can't compare {left_type:?} with {right_type:?}.").into()),
            },
            BinaryOperation::Less => match (left_type, right_type) {
                (Type::Number, Type::Number) | (Type::String, Type::String) => Ok(Type::Boolean),
                _ => Err(format!("Can't compare {left_type:?} with {right_type:?}.").into()),
            },
            BinaryOperation::LessEqual => match (left_type, right_type) {
                (Type::Number, Type::Number) | (Type::String, Type::String) => Ok(Type::Boolean),
                _ => Err(format!("Can't compare {left_type:?} with {right_type:?}.").into()),
            },
            BinaryOperation::Greater => match (left_type, right_type) {
                (Type::Number, Type::Number) | (Type::String, Type::String) => Ok(Type::Boolean),
                _ => Err(format!("Can't compare {left_type:?} with {right_type:?}.").into()),
            },
            BinaryOperation::GreaterEqual => match (left_type, right_type) {
                (Type::Number, Type::Number) | (Type::String, Type::String) => Ok(Type::Boolean),
                _ => Err(format!("Can't compare {left_type:?} with {right_type:?}.").into()),
            },
            BinaryOperation::Or => match (left_type, right_type) {
                (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
                _ => Err(
                    format!("Can't apply || operator on {left_type:?} by {right_type:?}.").into(),
                ),
            },
            BinaryOperation::And => match (left_type, right_type) {
                (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
                _ => Err(
                    format!("Can't apply && operator on {left_type:?} by {right_type:?}.").into(),
                ),
            },
            BinaryOperation::Assignment => {
                if left_type == right_type {
                    Ok(Type::Boolean)
                } else {
                    Err(format!("Can't assign {right_type:?} to {left_type:?}.").into())
                }
            }
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
    Tuple(Vec<Expression>),
    TupleAccess {
        expression: Box<Expression>,
        index: usize,
    },
}

pub struct Expression {
    pub value_type: Type,
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

pub fn get_type(variable: &String, stack: &[HashMap<String, Type>]) -> Option<Type> {
    for map in stack.iter().rev() {
        if let Some(value_type) = map.get(variable) {
            return Some(value_type.clone());
        }
    }

    None
}

pub fn set_type(variable: &str, value_type: Type, stack: &mut [HashMap<String, Type>]) {
    if let Some(map) = stack.iter_mut().next_back() {
        map.insert(variable.to_owned(), value_type);
    }
}

pub fn parse(
    tokens: &Vec<Token>,
    global_variables: &HashMap<String, Type>,
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
    variables: &mut Vec<HashMap<String, Type>>,
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
    variables: &mut Vec<HashMap<String, Type>>,
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
                    let variable_type = parse_type(tokens, current_token, errors)?;
                    Some(variable_type)
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

            set_type(&variable, expression.value_type.clone(), variables);
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

            if expression.value_type != Type::Boolean {
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

            if expression.value_type != Type::Boolean {
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

fn parse_type(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<Box<dyn Error>>,
) -> Option<Type> {
    match tokens.get(*current_token).map(|token| &token.token_type) {
        Some(TokenType::Number) => {
            *current_token += 1;
            Some(Type::Number)
        }
        Some(TokenType::String) => {
            *current_token += 1;
            Some(Type::String)
        }
        Some(TokenType::Bool) => {
            *current_token += 1;
            Some(Type::Boolean)
        }
        Some(TokenType::LeftParenthesis) => {
            *current_token += 1;

            let mut types = vec![];
            loop {
                let tuple_type = parse_type(tokens, current_token, errors)?;
                types.push(tuple_type);

                match tokens.get(*current_token).map(|token| &token.token_type) {
                    Some(TokenType::RightParenthesis) => {
                        *current_token += 1;
                        if types.len() == 1 {
                            errors.push("Tuple type must have at least two elements.".into());
                            return None;
                        }
                        return Some(Type::Tuple(types));
                    }
                    Some(TokenType::Comma) => {
                        *current_token += 1;
                    }
                    _ => {
                        errors.push("Can't parse type.".into());
                        return None;
                    }
                }
            }
        }
        _ => {
            errors.push("Can't parse type.".into());
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
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, Type>>,
) -> Option<Expression> {
    parse_assignment(tokens, current_token, errors, variables)
}

fn parse_assignment(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, Type>>,
) -> Option<Expression> {
    let Some(left_expression) = parse_or(tokens, current_token, errors, variables) else {
        return None;
    };

    if tokens.get(*current_token).map(|token| &token.token_type) != Some(&TokenType::Equal) {
        return Some(left_expression);
    }
    *current_token += 1;

    let Some(right_expression) = parse_assignment(tokens, current_token, errors, variables) else {
        return None;
    };

    match left_expression.expression_type {
        ExpressionType::Variable(_) | ExpressionType::TupleAccess { .. } => {}
        _ => {
            errors.push("Can only assign to variable or tuple elements.".into());
            return None;
        }
    }

    let value_type = match BinaryOperation::Assignment
        .value_type(&left_expression.value_type, &right_expression.value_type)
    {
        Ok(value_type) => value_type,
        Err(err) => {
            errors.push(err);
            return None;
        }
    };

    Some(Expression {
        value_type: value_type.clone(),
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
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, Type>>,
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
            match operation.value_type(&expression.value_type, &right_expression.value_type) {
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
    variables: &mut Vec<HashMap<String, Type>>,
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
            match operation.value_type(&expression.value_type, &right_expression.value_type) {
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
    variables: &mut Vec<HashMap<String, Type>>,
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
            match operation.value_type(&expression.value_type, &right_expression.value_type) {
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
    variables: &mut Vec<HashMap<String, Type>>,
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
            match operation.value_type(&expression.value_type, &right_expression.value_type) {
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
    variables: &mut Vec<HashMap<String, Type>>,
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
            match operation.value_type(&expression.value_type, &right_expression.value_type) {
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
    variables: &mut Vec<HashMap<String, Type>>,
) -> Option<Expression> {
    let operation = match tokens.get(*current_token).map(|token| &token.token_type) {
        Some(TokenType::Minus) => UnaryOperation::Minus,
        Some(TokenType::Exclamation) => UnaryOperation::Not,
        _ => return parse_tuple_access(tokens, current_token, errors, variables),
    };

    *current_token += 1;

    let Some(expression) = parse_unary(tokens, current_token, errors, variables) else {
        return None;
    };

    let value_type = match operation.value_type(&expression.value_type) {
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

fn parse_tuple_access(
    tokens: &Vec<Token>,
    current_token: &mut usize,
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, Type>>,
) -> Option<Expression> {
    let Some(mut expression) = parse_primary(tokens, current_token, errors, variables) else {
        return None;
    };

    loop {
        if tokens.get(*current_token).map(|token| &token.token_type) != Some(&TokenType::Dot) {
            return Some(expression);
        }

        *current_token += 1;

        *current_token += 1;
        let index = if let Some(TokenType::Literal(Value::Number(number))) = tokens
            .get(*current_token - 1)
            .map(|token| &token.token_type)
        {
            if number.floor() != *number {
                errors.push("Expected nonnegative integer index.".into());
                return None;
            }

            number.floor() as usize // TODO: Consider overflow
        } else {
            errors.push("Expected index after dot.".into());
            return None;
        };

        let value_type = if let Type::Tuple(types) = &expression.value_type {
            if index < types.len() {
                types[index].clone()
            } else {
                errors.push("Index too large".into());
                return None;
            }
        } else {
            errors.push("Tuple required for tuple access.".into());
            return None;
        };

        expression = Expression {
            value_type,
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
    errors: &mut Vec<Box<dyn Error>>,
    variables: &mut Vec<HashMap<String, Type>>,
) -> Option<Expression> {
    match tokens.get(*current_token) {
        None => {
            errors.push("No tokens available to parse".into());
            None
        }
        Some(token) => match &token.token_type {
            TokenType::Literal(value) => {
                *current_token += 1;
                let value_type = value.value_type();
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
                    return Some(Expression {
                        value_type: expression.value_type.clone(),
                        expression_type: ExpressionType::Grouping(expression.into()),
                    });
                }

                let mut expressions = vec![expression];
                loop {
                    if tokens.get(*current_token).map(|token| &token.token_type)
                        != Some(&TokenType::Comma)
                    {
                        errors.push("Expected , in tuple.".into());
                        return None;
                    }
                    *current_token += 1;

                    let Some(expression) = parse_expression(tokens, current_token, errors, variables) else { return None };
                    expressions.push(expression);

                    if tokens.get(*current_token).map(|token| &token.token_type)
                        == Some(&TokenType::RightParenthesis)
                    {
                        *current_token += 1;
                        return Some(Expression {
                            value_type: Type::Tuple(
                                expressions
                                    .iter()
                                    .map(|expression| expression.value_type.clone())
                                    .collect(),
                            ),
                            expression_type: ExpressionType::Tuple(expressions),
                        });
                    }
                }
            }
            _ => {
                errors.push("No tokens available to parse".into());
                None
            }
        },
    }
}
