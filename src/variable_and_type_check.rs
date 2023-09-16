use std::collections::HashMap;

use crate::{
    interpreter::Environment,
    parser::{
        BinaryOperation, CompilerError, Expression, ExpressionType, Statement, StatementType,
        UnaryOperation,
    },
    tokenizer::Type,
};

pub fn check_types(
    statements: &mut Vec<Statement>,
    global_variables: &Environment,
) -> Result<(), Vec<CompilerError>> {
    let mut errors = vec![];
    let mut global_types: HashMap<String, (usize, Type)> = HashMap::new();
    for ((name, shadow_id), value) in global_variables.variables.iter() {
        if !global_types.contains_key(name) || global_types[name].0 < *shadow_id {
            global_types.insert(name.clone(), (*shadow_id, value.value_type()));
        }
    }
    let mut stack = vec![global_types.clone()];

    for statement in statements {
        check_statement_type(statement, &mut errors, &mut stack, None);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn check_statement_type(
    statement: &mut Statement,
    errors: &mut Vec<CompilerError>,
    stack: &mut Vec<HashMap<String, (usize, Type)>>,
    current_function_declaration_return_type: Option<&Type>, // Are we in a function definition, if so what is that function's return type?
) -> bool {
    // Will this statement necessarily return something?
    match &mut statement.statement {
        StatementType::Block(statements) => {
            let mut will_return = false;
            stack.push(HashMap::new());
            for statement in statements {
                let statement_will_return = check_statement_type(
                    statement,
                    errors,
                    stack,
                    current_function_declaration_return_type,
                );
                will_return = will_return || statement_will_return;
            }
            stack.pop();
            will_return
        }
        StatementType::Expression(expression) => {
            check_expression_type(expression, errors, stack);
            false
        }
        StatementType::If {
            expression,
            then_statement,
            else_statement,
        } => {
            let then_will_return = check_statement_type(
                then_statement,
                errors,
                stack,
                current_function_declaration_return_type,
            );
            let else_will_return = if let Some(else_statement) = else_statement {
                check_statement_type(
                    else_statement,
                    errors,
                    stack,
                    current_function_declaration_return_type,
                )
            } else {
                false
            };

            let Some(expression_type) = check_expression_type(expression, errors, stack) else {
                return then_will_return && else_will_return;
            };
            if expression_type != Type::Boolean {
                errors.push(CompilerError {
                    lines: expression.lines,
                    error: "Boolean expression expected for if condition.".into(),
                });
            }
            then_will_return && else_will_return
        }
        StatementType::VariableDeclaration {
            variable,
            variable_type,
            value,
            shadow_id,
        } => {
            let Some(value_type) = check_expression_type(value, errors, stack) else {
                return false;
            };

            if let Some(variable_type) = variable_type {
                if *variable_type != value_type {
                    errors.push(CompilerError {
                        lines: statement.lines,
                        error: format!("Expression of type {value_type:?} can't be assigned to a variable of type {variable_type:?}.")
                    });
                    return false;
                }
            }

            let new_shadow_id = set_type(variable, value_type, stack);
            *shadow_id = Some(new_shadow_id);
            false
        }
        StatementType::While {
            expression,
            statement,
        } => {
            check_statement_type(
                statement,
                errors,
                stack,
                current_function_declaration_return_type,
            );

            let Some(expression_type) = check_expression_type(expression, errors, stack) else {
                return false;
            };
            if expression_type != Type::Boolean {
                errors.push(CompilerError {
                    lines: expression.lines,
                    error: "Boolean expression expected for while condition.".into(),
                })
            }
            false // TODO: Maybe allow infinite loops to return true - similar to how Rust handles infinite loops.
        }
        StatementType::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
            shadow_id,
        } => {
            let new_shadow_id = set_type(
                name,
                Type::Function {
                    parameters: parameters
                        .iter()
                        .map(|(_, _, parameter_type)| parameter_type.clone())
                        .collect(),
                    return_type: return_type.clone().into(),
                },
                stack,
            );
            *shadow_id = Some(new_shadow_id);

            stack.push(HashMap::new());
            for (parameter, shadow_id, parameter_type) in parameters.iter_mut() {
                let new_shadow_id = set_type(parameter, parameter_type.clone(), stack);
                *shadow_id = Some(new_shadow_id);
            }
            let body_will_return = check_statement_type(body, errors, stack, Some(return_type));
            stack.pop();

            if !body_will_return && return_type != &Type::Void {
                errors.push(CompilerError {
                    lines: statement.lines,
                    error: "Function body may not return".into(),
                });
            }
            false
        }
        StatementType::Return(expression) => {
            let expression_type = if let Some(expression) = expression {
                let Some(expression_type) = check_expression_type(expression, errors, stack) else {
                    return false;
                };
                expression_type
            } else {
                Type::Void
            };

            if let Some(return_type) = current_function_declaration_return_type {
                if expression_type != *return_type {
                    // TODO: Consider Any?
                    errors.push(CompilerError {
                        lines: statement.lines,
                        error: format!("Expected {return_type:?} return, got {expression_type:?}."),
                    });
                    false
                } else {
                    true
                }
            } else {
                errors.push(CompilerError {
                    lines: statement.lines,
                    error: "Return can't be used outside a function.".into(),
                });
                false
            }
        }
    }
}

fn check_expression_type(
    expression: &mut Expression,
    errors: &mut Vec<CompilerError>,
    stack: &mut Vec<HashMap<String, (usize, Type)>>,
) -> Option<Type> {
    match &mut expression.expression_type {
        ExpressionType::Binary {
            operation,
            left_expression,
            right_expression,
        } => {
            let left_type = check_expression_type(left_expression, errors, stack);
            let right_type = check_expression_type(right_expression, errors, stack);

            match operation.result_type(
                left_expression,
                &left_type?,
                right_expression,
                &right_type?,
            ) {
                Ok(value_type) => Some(value_type),
                Err(err) => {
                    errors.push(err);
                    None
                }
            }
        }
        ExpressionType::Grouping(expression) => check_expression_type(expression, errors, stack),
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
            let expression_type = check_expression_type(expression, errors, stack)?;
            match operation.result_type(expression, &expression_type) {
                Ok(value_type) => Some(value_type),
                Err(err) => {
                    errors.push(err);
                    None
                }
            }
        }
        ExpressionType::Variable {
            name,
            shadow_id,
            parent_height,
        } => match get_type(name, stack) {
            Some((current_shadow_id, variable_parent_height, variable_type)) => {
                *shadow_id = Some(current_shadow_id);
                *parent_height = Some(variable_parent_height);
                Some(variable_type)
            }
            None => {
                errors.push(CompilerError {
                    lines: expression.lines,
                    error: format!("No variable called {name} exists."),
                });
                None
            }
        },
        ExpressionType::FunctionCall {
            function,
            arguments,
        } => {
            let Type::Function { return_type, parameters } = check_expression_type(function, errors, stack)? else {
                errors.push(CompilerError { lines: expression.lines, error: format!("{function} is not a function") });
                return None;
            };

            let mut argument_types = vec![];
            let mut valid_argument_types = true;
            for argument in arguments {
                match check_expression_type(argument, errors, stack) {
                    Some(argument_type) => {
                        argument_types.push(argument_type);
                    }
                    None => {
                        valid_argument_types = false;
                    }
                }
            }

            if !valid_argument_types {
                return None;
            }

            if argument_types.len() != parameters.len() {
                errors.push(CompilerError {
                    lines: expression.lines,
                    error: format!("Invalid function calls.\nFunction parameters: {parameters:?}\nArguments passed: {argument_types:?}")
                });
                return None;
            }

            for (argument_type, parameter_type) in argument_types.iter().zip(parameters.iter()) {
                if !can_assign(parameter_type, argument_type) {
                    errors.push(CompilerError {
                        lines: expression.lines,
                        error: format!("Invalid function calls.\nFunction parameters: {parameters:?}\nArguments passed: {argument_types:?}")
                    });
                    return None;
                }
            }

            Some(*return_type)
        }
    }
}

fn can_assign(left_type: &Type, right_type: &Type) -> bool {
    match (left_type, right_type) {
        (Type::Any, _)
        | (Type::Boolean, Type::Boolean)
        | (Type::Number, Type::Number)
        | (Type::String, Type::String)
        | (Type::Void, Type::Void) => true,
        (Type::Tuple(left_types), Type::Tuple(right_types)) => {
            if left_types.len() != right_types.len() {
                return false;
            }

            for (left_type, right_type) in left_types.iter().zip(right_types.iter()) {
                if !can_assign(left_type, right_type) {
                    return false;
                }
            }

            true
        }
        (
            Type::Function {
                parameters: left_parameters,
                return_type: left_return_type,
            },
            Type::Function {
                parameters: right_parameters,
                return_type: right_return_type,
            },
        ) => {
            if left_parameters.len() != right_parameters.len() {
                return false;
            }

            for (left_parameter_type, right_parameter_type) in
                left_parameters.iter().zip(right_parameters.iter())
            {
                if !can_assign(right_parameter_type, left_parameter_type) {
                    return false;
                }
            }

            if !can_assign(left_return_type, right_return_type) {
                return false;
            }

            true
        }
        _ => false,
    }
}

fn can_check_equality(left_type: &Type, right_type: &Type) -> bool {
    // TODO: Function type check?
    match (left_type, right_type) {
        (Type::Any, _)
        | (_, Type::Any)
        | (Type::Boolean, Type::Boolean)
        | (Type::Number, Type::Number)
        | (Type::String, Type::String)
        | (Type::Void, Type::Void) => true,
        (Type::Tuple(left_types), Type::Tuple(right_types)) => {
            if left_types.len() != right_types.len() {
                return false;
            }

            for (left_type, right_type) in left_types.iter().zip(right_types.iter()) {
                if !can_check_equality(left_type, right_type) {
                    return false;
                }
            }

            true
        }
        (
            Type::Function {
                parameters: left_parameters,
                return_type: left_return_type,
            },
            Type::Function {
                parameters: right_parameters,
                return_type: right_return_type,
            },
        ) => {
            if left_parameters.len() != right_parameters.len() {
                return false;
            }

            for (left_parameter_type, right_parameter_type) in
                left_parameters.iter().zip(right_parameters.iter())
            {
                if !can_check_equality(left_parameter_type, right_parameter_type) {
                    return false;
                }
            }

            if !can_check_equality(left_return_type, right_return_type) {
                return false;
            }

            true
        }
        _ => false,
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
            BinaryOperation::Equal => {
                if can_check_equality(left_type, right_type) {
                    Ok(Type::Boolean)
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
            BinaryOperation::NotEqual => {
                if can_check_equality(left_type, right_type) {
                    Ok(Type::Boolean)
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
                // TODO: Handle any and void?
                match left_expression.expression_type {
                    ExpressionType::Variable { .. } | ExpressionType::TupleAccess { .. } => {}
                    _ => {
                        return Err(CompilerError {
                            lines: (left_expression.lines.0, right_expression.lines.1),
                            error: "Can only assign to variable or tuple elements.".into(),
                        });
                    }
                }

                if can_assign(left_type, right_type) {
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

pub fn get_type(
    variable: &String,
    stack: &[HashMap<String, (usize, Type)>],
) -> Option<(usize, usize, Type)> {
    for (height, map) in stack.iter().rev().enumerate() {
        if let Some(value_type) = map.get(variable) {
            let res = value_type.clone();
            return Some((res.0, height, res.1));
        }
    }

    None
}

pub fn set_type(
    variable: &str,
    value_type: Type,
    stack: &mut [HashMap<String, (usize, Type)>],
) -> usize {
    let map = stack.last_mut().unwrap(); // We can unwrap as there will always be a global environment.
    if map.contains_key(variable) {
        let previous_shadow_id = map[variable].0;
        map.insert(variable.to_owned(), (previous_shadow_id + 1, value_type));
        previous_shadow_id + 1
    } else {
        map.insert(variable.to_owned(), (0, value_type));
        0
    }
}
