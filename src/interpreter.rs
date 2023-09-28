use std::collections::{HashMap, HashSet, VecDeque};

use crate::{
    parser::{
        BinaryOperation, Expression, ExpressionType, Statement, StatementType, UnaryOperation,
    },
    tokenizer::{FunctionBody, Value},
};

pub struct Environment {
    parent: Option<usize>,
    pub variables: HashMap<(String, usize), Value>,
}

pub struct Variables {
    current_environment: usize,
    pub environments: HashMap<usize, Environment>,
    current_environments: Vec<usize>,
    garbage_collection_counter: usize,
    last_id: usize,
}

impl Variables {
    pub fn new() -> Variables {
        let mut environments = HashMap::new();
        environments.insert(
            0,
            Environment {
                parent: None,
                variables: HashMap::new(),
            },
        );
        Variables {
            current_environment: 0,
            environments,
            last_id: 0,
            current_environments: vec![],
            garbage_collection_counter: 100,
        }
    }

    fn add_reachable_environments(
        queued_environments: &mut HashSet<usize>,
        environment_queue: &mut VecDeque<usize>,
        value: &Value,
    ) {
        match value {
            Value::Function {
                parent_environment, ..
            } => {
                if !queued_environments.contains(parent_environment) {
                    environment_queue.push_back(*parent_environment);
                    queued_environments.insert(*parent_environment);
                }
            }
            Value::Tuple(values) => {
                for value in values {
                    Variables::add_reachable_environments(
                        queued_environments,
                        environment_queue,
                        value,
                    );
                }
            }
            _ => {}
        }
    }

    fn collect_garbage(&mut self) {
        let mut queued_environments = HashSet::new();
        let mut environment_queue = VecDeque::new();

        environment_queue.extend(self.current_environments.iter());
        queued_environments.extend(self.current_environments.iter());

        environment_queue.push_back(self.current_environment);
        queued_environments.insert(self.current_environment);

        while let Some(environment_id) = environment_queue.pop_front() {
            let environment = &self.environments[&environment_id];

            if let Some(parent_environment) = environment.parent {
                if !queued_environments.contains(&parent_environment) {
                    environment_queue.push_back(parent_environment);
                    queued_environments.insert(parent_environment);
                }
            }

            for value in environment.variables.values() {
                Variables::add_reachable_environments(
                    &mut queued_environments,
                    &mut environment_queue,
                    value,
                );
            }
        }

        let non_queued_environments = self
            .environments
            .keys()
            .copied()
            .filter(|key| !queued_environments.contains(key))
            .collect::<Vec<_>>();
        for environment_id in non_queued_environments {
            self.environments.remove(&environment_id);
        }

        self.garbage_collection_counter = 100;
    }

    fn push_environment(&mut self) {
        self.environments.insert(
            self.last_id + 1,
            Environment {
                parent: Some(self.current_environment),
                variables: HashMap::new(),
            },
        );
        self.current_environment = self.last_id + 1;
        self.last_id += 1;

        self.garbage_collection_counter -= 1;
        if self.garbage_collection_counter == 0 {
            self.collect_garbage();
        }
    }

    fn pop_environment(&mut self) {
        self.current_environment = self.environments[&self.current_environment].parent.unwrap();
    }

    fn push_function_environment(&mut self, environment: usize) {
        self.current_environments.push(self.current_environment);
        self.current_environment = environment;
    }

    fn pop_function_environment(&mut self) {
        self.current_environment = self.current_environments.pop().unwrap();
    }

    fn get_variable(
        &self,
        variable: &str,
        shadow_id: usize,
        parent_height: usize,
    ) -> Option<Value> {
        let mut current_environment = self.current_environment;
        for _ in 0..parent_height {
            if let Some(environment) = self.environments[&current_environment].parent {
                current_environment = environment;
            } else {
                return None;
            }
        }

        self.environments[&current_environment]
            .variables
            .get(&(variable.to_owned(), shadow_id))
            .cloned()
    }

    /// Sets a variable. Doesn't create a new one.
    fn set_variable(
        &mut self,
        variable: &str,
        shadow_id: usize,
        parent_height: usize,
        value: Value,
    ) -> Result<(), ()> {
        let mut current_environment = self.current_environment;
        for _ in 0..parent_height {
            if let Some(environment) = self.environments[&current_environment].parent {
                current_environment = environment;
            } else {
                return Err(());
            }
        }

        if self.environments[&current_environment]
            .variables
            .contains_key(&(variable.to_owned(), shadow_id))
        {
            self.environments
                .get_mut(&current_environment)
                .unwrap()
                .variables
                .insert((variable.to_owned(), shadow_id), value);
            Ok(())
        } else {
            Err(())
        }
    }

    fn create_variable(&mut self, variable: &str, shadow_id: usize, value: Value) {
        self.environments
            .get_mut(&self.current_environment)
            .unwrap()
            .variables
            .insert((variable.to_owned(), shadow_id), value);
    }
}

pub fn interpret(statements: &Vec<Statement>, variables: &mut Variables) {
    for statement in statements {
        interpret_statement(statement, variables);
    }
}

fn interpret_statement(statement: &Statement, variables: &mut Variables) -> Option<Value> {
    // Return value
    match &statement.statement {
        StatementType::Expression(expression) => {
            interpret_expression(expression, variables);
            None
        }
        StatementType::VariableDeclaration {
            variable,
            value,
            shadow_id,
            ..
        } => {
            let value = interpret_expression(value, variables);
            variables.create_variable(variable, shadow_id.unwrap(), value);
            None
        }
        StatementType::Block(statements) => {
            variables.push_environment();
            for statement in statements {
                if let Some(value) = interpret_statement(statement, variables) {
                    variables.pop_environment();
                    return Some(value);
                }
            }
            variables.pop_environment();
            None
        }
        StatementType::If {
            expression,
            then_statement,
            else_statement,
        } => {
            let Value::Boolean(value) = interpret_expression(expression, variables) else {
                unreachable!();
            };

            if value {
                if let Some(value) = interpret_statement(then_statement, variables) {
                    return Some(value);
                }
            } else if let Some(else_statement) = else_statement {
                if let Some(value) = interpret_statement(else_statement, variables) {
                    return Some(value);
                }
            }

            None
        }
        StatementType::While {
            expression,
            statement,
        } => loop {
            let Value::Boolean(run_loop) = interpret_expression(expression, variables) else {
                    unreachable!();
                };

            if run_loop {
                if let Some(value) = interpret_statement(statement, variables) {
                    return Some(value);
                }
            } else {
                return None;
            }
        },
        StatementType::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
            shadow_id,
        } => {
            variables.create_variable(
                name,
                shadow_id.unwrap(),
                Value::Function {
                    parameters: parameters.clone(),
                    return_type: return_type.clone(),
                    body: FunctionBody::Statement(body.clone()),
                    parent_environment: variables.current_environment,
                },
            );
            None
        }
        StatementType::Return(expression) => {
            if let Some(expression) = expression {
                Some(interpret_expression(expression, variables))
            } else {
                Some(Value::Void)
            }
        }
    }
}

fn interpret_expression(expression: &Expression, variables: &mut Variables) -> Value {
    match &expression.expression_type {
        ExpressionType::Unary {
            operation,
            expression,
        } => match operation {
            UnaryOperation::Minus => {
                let expression_value = interpret_expression(expression, variables);
                if let Value::Number(number) = expression_value {
                    Value::Number(-number)
                } else {
                    unreachable!()
                }
            }
            UnaryOperation::Not => {
                let expression_value = interpret_expression(expression, variables);
                if let Value::Boolean(boolean) = expression_value {
                    Value::Boolean(!boolean)
                } else {
                    unreachable!()
                }
            }
        },
        ExpressionType::Binary {
            operation,
            left_expression,
            right_expression,
        } => match operation {
            BinaryOperation::Add => {
                let left_value = interpret_expression(left_expression, variables);
                let right_value = interpret_expression(right_expression, variables);
                match (left_value, right_value) {
                    (Value::Number(left), Value::Number(right)) => Value::Number(left + right),
                    (Value::String(left), Value::String(right)) => Value::String(left + &right),
                    _ => {
                        unreachable!()
                    }
                }
            }
            BinaryOperation::Subtract => {
                let left_value = interpret_expression(left_expression, variables);
                let right_value = interpret_expression(right_expression, variables);
                match (left_value, right_value) {
                    (Value::Number(left), Value::Number(right)) => Value::Number(left - right),
                    _ => {
                        unreachable!()
                    }
                }
            }
            BinaryOperation::Multiply => {
                let left_value = interpret_expression(left_expression, variables);
                let right_value = interpret_expression(right_expression, variables);
                match (left_value, right_value) {
                    (Value::Number(left), Value::Number(right)) => Value::Number(left * right),
                    _ => {
                        unreachable!()
                    }
                }
            }
            BinaryOperation::Divide => {
                let left_value = interpret_expression(left_expression, variables);
                let right_value = interpret_expression(right_expression, variables);
                match (left_value, right_value) {
                    (Value::Number(left), Value::Number(right)) => Value::Number(left / right),
                    _ => {
                        unreachable!()
                    }
                }
            }
            BinaryOperation::Equal => {
                let left_value = interpret_expression(left_expression, variables);
                let right_value = interpret_expression(right_expression, variables);
                Value::Boolean(left_value == right_value)
            }
            BinaryOperation::NotEqual => {
                let left_value = interpret_expression(left_expression, variables);
                let right_value = interpret_expression(right_expression, variables);
                Value::Boolean(left_value != right_value)
            }
            BinaryOperation::Less => {
                let left_value = interpret_expression(left_expression, variables);
                let right_value = interpret_expression(right_expression, variables);
                match (left_value, right_value) {
                    (Value::Number(left), Value::Number(right)) => Value::Boolean(left < right),
                    (Value::String(left), Value::String(right)) => Value::Boolean(left < right),
                    _ => {
                        unreachable!()
                    }
                }
            }
            BinaryOperation::LessEqual => {
                let left_value = interpret_expression(left_expression, variables);
                let right_value = interpret_expression(right_expression, variables);
                match (left_value, right_value) {
                    (Value::Number(left), Value::Number(right)) => Value::Boolean(left <= right),
                    (Value::String(left), Value::String(right)) => Value::Boolean(left <= right),
                    _ => {
                        unreachable!()
                    }
                }
            }
            BinaryOperation::Greater => {
                let left_value = interpret_expression(left_expression, variables);
                let right_value = interpret_expression(right_expression, variables);
                match (left_value, right_value) {
                    (Value::Number(left), Value::Number(right)) => Value::Boolean(left > right),
                    (Value::String(left), Value::String(right)) => Value::Boolean(left > right),
                    _ => {
                        unreachable!()
                    }
                }
            }
            BinaryOperation::GreaterEqual => {
                let left_value = interpret_expression(left_expression, variables);
                let right_value = interpret_expression(right_expression, variables);
                match (left_value, right_value) {
                    (Value::Number(left), Value::Number(right)) => Value::Boolean(left >= right),
                    (Value::String(left), Value::String(right)) => Value::Boolean(left >= right),
                    _ => {
                        unreachable!()
                    }
                }
            }
            BinaryOperation::Or => {
                let left_value = interpret_expression(left_expression, variables);
                if left_value == Value::Boolean(true) {
                    left_value
                } else {
                    interpret_expression(right_expression, variables)
                }
            }
            BinaryOperation::And => {
                let left_value = interpret_expression(left_expression, variables);
                if left_value == Value::Boolean(false) {
                    left_value
                } else {
                    interpret_expression(right_expression, variables)
                }
            }
            BinaryOperation::Assignment => {
                let value = interpret_expression(right_expression, variables);
                match &left_expression.expression_type {
                    ExpressionType::Variable {
                        name,
                        shadow_id,
                        parent_height,
                    } => {
                        variables
                            .set_variable(
                                name,
                                shadow_id.unwrap(),
                                parent_height.unwrap(),
                                value.clone(),
                            )
                            .unwrap();
                        value
                    }
                    ExpressionType::TupleAccess { expression, index } => {
                        // We access the tuple field and mutate it if the field is in a variable.
                        let mut current_expression = expression;
                        let mut indices = vec![*index];
                        loop {
                            match &current_expression.expression_type {
                                ExpressionType::TupleAccess { expression, index } => {
                                    current_expression = expression;
                                    indices.push(*index);
                                }
                                ExpressionType::Variable {
                                    name,
                                    shadow_id,
                                    parent_height,
                                } => {
                                    let mut variable_value = variables
                                        .get_variable(
                                            name,
                                            shadow_id.unwrap(),
                                            parent_height.unwrap(),
                                        )
                                        .unwrap();
                                    let mut lvalue = &mut variable_value;
                                    for &index in indices.iter().rev() {
                                        let Value::Tuple(values) = lvalue else { unreachable!() };
                                        lvalue = &mut values[index];
                                    }
                                    *lvalue = value.clone();
                                    variables
                                        .set_variable(
                                            name,
                                            shadow_id.unwrap(),
                                            parent_height.unwrap(),
                                            variable_value,
                                        )
                                        .unwrap();
                                    break;
                                }
                                _ => {
                                    // NOTE: may change when I add pointers.
                                    interpret_expression(expression, variables);
                                    break;
                                }
                            }
                        }
                        value
                    }
                    _ => {
                        unreachable!()
                    }
                }
            }
        },
        ExpressionType::Variable {
            name,
            shadow_id,
            parent_height,
        } => variables
            .get_variable(name, shadow_id.unwrap(), parent_height.unwrap())
            .unwrap(),
        ExpressionType::Literal(value) => value.clone(),
        ExpressionType::Grouping(expression) => interpret_expression(expression, variables),
        ExpressionType::Tuple(expressions) => Value::Tuple(
            expressions
                .iter()
                .map(|expression| interpret_expression(expression, variables))
                .collect(),
        ),
        ExpressionType::TupleAccess { expression, index } => {
            let Value::Tuple(values) = interpret_expression(expression, variables) else {
                unreachable!()
            };
            values[*index].clone()
        }
        ExpressionType::FunctionCall {
            function,
            arguments,
        } => {
            let Value::Function { parameters, body, parent_environment , ..} = interpret_expression(function, variables) else {
                unreachable!();
            };

            let mut argument_values = vec![];
            for argument in arguments {
                argument_values.push(interpret_expression(argument, variables));
            }

            match body {
                FunctionBody::Statement(statement) => {
                    variables.push_function_environment(parent_environment);
                    variables.push_environment();
                    for ((parameter, shadow_id), value) in parameters
                        .into_iter()
                        .map(|(parameter, shadow_id, _)| (parameter, shadow_id))
                        .zip(argument_values.into_iter())
                    {
                        variables.create_variable(&parameter, shadow_id.unwrap(), value);
                    }
                    let return_value =
                        interpret_statement(&statement, variables).unwrap_or(Value::Void);
                    variables.pop_environment();
                    variables.pop_function_environment();
                    return_value
                }
                FunctionBody::RustClosure { closure, .. } => closure(argument_values),
            } // TODO: Handle return types
        }
    }
}
