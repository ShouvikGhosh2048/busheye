use std::collections::HashMap;

use crate::{
    parser::{
        BinaryOperation, CompilerError, Expression, ExpressionType, Statement, StatementType,
        UnaryOperation,
    },
    tokenizer::Type,
};

pub fn check_types(
    statements: &Vec<Statement>,
    global_variables: &HashMap<String, Type>,
) -> Result<(), Vec<CompilerError>> {
    let mut errors = vec![];
    let mut stack = vec![global_variables.clone()];

    for statement in statements {
        check_statement_type(statement, &mut errors, &mut stack);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn check_statement_type(
    statement: &Statement,
    errors: &mut Vec<CompilerError>,
    stack: &mut Vec<HashMap<String, Type>>,
) {
    match &statement.statement {
        StatementType::Block(statements) => {
            stack.push(HashMap::new());
            for statement in statements {
                check_statement_type(statement, errors, stack);
            }
            stack.pop();
        }
        StatementType::Expression(expression) => {
            check_expression_type(expression, errors, stack);
        }
        StatementType::If {
            expression,
            then_statement,
            else_statement,
        } => {
            check_statement_type(then_statement, errors, stack);
            if let Some(else_statement) = else_statement {
                check_statement_type(else_statement, errors, stack);
            }

            let Some(expression_type) = check_expression_type(expression, errors, stack) else {
                return;
            };
            if expression_type != Type::Boolean {
                errors.push(CompilerError {
                    lines: expression.lines,
                    error: "Boolean expression expected for if condition.".into(),
                })
            }
        }
        StatementType::VariableDeclaration {
            variable,
            variable_type,
            value,
        } => {
            let Some(value_type) = check_expression_type(value, errors, stack) else {
                return;
            };

            if let Some(variable_type) = variable_type {
                if *variable_type != value_type {
                    errors.push(CompilerError { 
                        lines: statement.lines,
                        error: format!("Expression of type {value_type:?} can't be assigned to a variable of type {variable_type:?}.")
                    });
                    return;
                }
            }

            set_type(variable, value_type, stack);
        }
        StatementType::While {
            expression,
            statement,
        } => {
            check_statement_type(statement, errors, stack);

            let Some(expression_type) = check_expression_type(expression, errors, stack) else {
                return;
            };
            if expression_type != Type::Boolean {
                errors.push(CompilerError {
                    lines: expression.lines,
                    error: "Boolean expression expected for while condition.".into(),
                })
            }
        }
    }
}

fn check_expression_type(
    expression: &Expression,
    errors: &mut Vec<CompilerError>,
    stack: &mut Vec<HashMap<String, Type>>,
) -> Option<Type> {
    match &expression.expression_type {
        ExpressionType::Binary {
            operation,
            left_expression,
            right_expression,
        } => {
            let left_type = check_expression_type(left_expression, errors, stack);
            let right_type = check_expression_type(right_expression, errors, stack);

            match operation.result_type(left_expression, &left_type?, right_expression, &right_type?)
            {
                Ok(value_type) => Some(value_type),
                Err(err) => {
                    errors.push(err);
                    None
                }
            }
        }
        ExpressionType::Grouping(expression) => {
            check_expression_type(expression, errors, stack)
        }
        ExpressionType::Literal(value) => Some(value.value_type()),
        ExpressionType::Tuple(expressions) => {
            let mut expression_types = vec![];
            let mut valid_types = true;

            for expression in expressions {
                let expression_type = check_expression_type(expression, errors, stack);
                if let Some(expression_type) = expression_type {
                    expression_types.push(expression_type);
                } else {
                    valid_types = false;
                }
            }

            if valid_types {
                Some(Type::Tuple(expression_types))
            } else {
                None
            }
        }
        ExpressionType::TupleAccess { expression, index } => {
            let Type::Tuple(tuple_type) = check_expression_type(expression, errors, stack)? else {
                errors.push(CompilerError { lines: expression.lines, error: "Tuple required for tuple access".into() });
                return None;
            };

            if *index < tuple_type.len() {
                Some(tuple_type[*index].clone())
            } else {
                errors.push(CompilerError {
                    lines: expression.lines,
                    error: "Index too large".into(),
                });
                None
            }
        }
        ExpressionType::Unary {
            operation,
            expression,
        } => {
            match operation.result_type(
                expression,
                &check_expression_type(expression, errors, stack)?,
            ) {
                Ok(value_type) => Some(value_type),
                Err(err) => {
                    errors.push(err);
                    None
                }
            }
        }
        ExpressionType::Variable(variable) => match get_type(variable, stack) {
            Some(variable_type) => Some(variable_type),
            None => {
                errors.push(CompilerError {
                    lines: expression.lines,
                    error: format!("No variable called {variable} exists."),
                });
                None
            }
        },
    }
}

impl UnaryOperation {
    pub fn result_type(
        &self,
        expression: &Expression,
        expression_type: &Type,
    ) -> Result<Type, CompilerError> {
        match self {
            UnaryOperation::Minus => match expression_type {
                Type::Number => Ok(Type::Number),
                _ => Err(unary_parse_error(self, expression, expression_type)),
            },
            UnaryOperation::Not => match expression_type {
                Type::Boolean => Ok(Type::Boolean),
                _ => Err(unary_parse_error(self, expression, expression_type)),
            },
        }
    }
}

fn unary_parse_error(
    operation: &UnaryOperation,
    expression: &Expression,
    expression_type: &Type,
) -> CompilerError {
    CompilerError {
        lines: expression.lines,
        error: format!(
            "Can't apply {operation:?} on {expression}.\nExpression type: {:?}",
            expression_type
        ),
    }
}

impl BinaryOperation {
    pub fn result_type(
        &self,
        left_expression: &Expression,
        left_type: &Type,
        right_expression: &Expression,
        right_type: &Type,
    ) -> Result<Type, CompilerError> {
        match self {
            BinaryOperation::Add => match (left_type, right_type) {
                (Type::Number, Type::Number) => Ok(Type::Number),
                (Type::String, Type::String) => Ok(Type::String),
                _ => Err(binary_non_matching_types_error(
                    self,
                    left_expression,
                    left_type,
                    right_expression,
                    right_type,
                )),
            },
            BinaryOperation::Subtract => match (left_type, right_type) {
                (Type::Number, Type::Number) => Ok(Type::Number),
                _ => Err(binary_non_matching_types_error(
                    self,
                    left_expression,
                    left_type,
                    right_expression,
                    right_type,
                )),
            },
            BinaryOperation::Multiply => match (left_type, right_type) {
                (Type::Number, Type::Number) => Ok(Type::Number),
                _ => Err(binary_non_matching_types_error(
                    self,
                    left_expression,
                    left_type,
                    right_expression,
                    right_type,
                )),
            },
            BinaryOperation::Divide => match (left_type, right_type) {
                (Type::Number, Type::Number) => Ok(Type::Number),
                _ => Err(binary_non_matching_types_error(
                    self,
                    left_expression,
                    left_type,
                    right_expression,
                    right_type,
                )),
            },
            BinaryOperation::Equal => match (left_type, right_type) {
                (Type::Number, Type::Number)
                | (Type::String, Type::String)
                | (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
                _ => Err(binary_non_matching_types_error(
                    self,
                    left_expression,
                    left_type,
                    right_expression,
                    right_type,
                )),
            },
            BinaryOperation::NotEqual => match (left_type, right_type) {
                (Type::Number, Type::Number)
                | (Type::String, Type::String)
                | (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
                _ => Err(binary_non_matching_types_error(
                    self,
                    left_expression,
                    left_type,
                    right_expression,
                    right_type,
                )),
            },
            BinaryOperation::Less => match (left_type, right_type) {
                (Type::Number, Type::Number) | (Type::String, Type::String) => Ok(Type::Boolean),
                _ => Err(binary_non_matching_types_error(
                    self,
                    left_expression,
                    left_type,
                    right_expression,
                    right_type,
                )),
            },
            BinaryOperation::LessEqual => match (left_type, right_type) {
                (Type::Number, Type::Number) | (Type::String, Type::String) => Ok(Type::Boolean),
                _ => Err(binary_non_matching_types_error(
                    self,
                    left_expression,
                    left_type,
                    right_expression,
                    right_type,
                )),
            },
            BinaryOperation::Greater => match (left_type, right_type) {
                (Type::Number, Type::Number) | (Type::String, Type::String) => Ok(Type::Boolean),
                _ => Err(binary_non_matching_types_error(
                    self,
                    left_expression,
                    left_type,
                    right_expression,
                    right_type,
                )),
            },
            BinaryOperation::GreaterEqual => match (left_type, right_type) {
                (Type::Number, Type::Number) | (Type::String, Type::String) => Ok(Type::Boolean),
                _ => Err(binary_non_matching_types_error(
                    self,
                    left_expression,
                    left_type,
                    right_expression,
                    right_type,
                )),
            },
            BinaryOperation::Or => match (left_type, right_type) {
                (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
                _ => Err(binary_non_matching_types_error(
                    self,
                    left_expression,
                    left_type,
                    right_expression,
                    right_type,
                )),
            },
            BinaryOperation::And => match (left_type, right_type) {
                (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
                _ => Err(binary_non_matching_types_error(
                    self,
                    left_expression,
                    left_type,
                    right_expression,
                    right_type,
                )),
            },
            BinaryOperation::Assignment => {
                match left_expression.expression_type {
                    ExpressionType::Variable(_) | ExpressionType::TupleAccess { .. } => {}
                    _ => {
                        return Err(CompilerError {
                            lines: (left_expression.lines.0, right_expression.lines.1),
                            error: "Can only assign to variable or tuple elements.".into(),
                        });
                    }
                }

                if left_type == right_type {
                    Ok(right_type.clone())
                } else {
                    Err(binary_non_matching_types_error(
                        self,
                        left_expression,
                        left_type,
                        right_expression,
                        right_type,
                    ))
                }
            }
        }
    }
}

fn binary_non_matching_types_error(
    operation: &BinaryOperation,
    left_expression: &Expression,
    left_type: &Type,
    right_expression: &Expression,
    right_type: &Type,
) -> CompilerError {
    CompilerError {
        lines: (left_expression.lines.0, right_expression.lines.1),
        error: format!(
            "Can't apply {operation:?} on {left_expression} and {right_expression}.\nLeft type: {:?}\nRight type: {:?}",
            left_type,
            right_type
        )
    }
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
