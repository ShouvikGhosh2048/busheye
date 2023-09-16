mod interpreter;
mod parser;
mod tokenizer;
mod variable_and_type_check;

use std::{
    env, fs,
    io::{self, Write},
    rc::Rc,
};

use interpreter::Variables;
use tokenizer::{FunctionBody, Type, Value};
use variable_and_type_check::check_types;

fn run(program: &str, variables: &mut Variables) {
    let tokens = match tokenizer::tokenize(program) {
        Ok(tokens) => tokens,
        Err(errors) => {
            for error in errors {
                println!("{error}");
            }
            return;
        }
    };

    let mut statements = match parser::parse(&tokens) {
        Ok(statements) => statements,
        Err(errors) => {
            let lines = program.lines().collect::<Vec<_>>();
            for error in errors {
                let index_width = error.lines.1.ilog10() + 1;
                for (line_index, line) in lines
                    .iter()
                    .enumerate()
                    .take(error.lines.1)
                    .skip(error.lines.0 - 1)
                {
                    print!("{}", line_index + 1);
                    for _ in 0..(index_width - (line_index + 1).ilog10()) {
                        print!(" ");
                    }
                    println!("| {}", line);
                }
                println!("{}", error.error);
                println!();
            }
            return;
        }
    };

    if let Err(errors) = check_types(&mut statements, &variables.environments[&0]) {
        let lines = program.lines().collect::<Vec<_>>();
        for error in errors {
            let index_width = error.lines.1.ilog10() + 1;
            for (line_index, line) in lines
                .iter()
                .enumerate()
                .take(error.lines.1)
                .skip(error.lines.0 - 1)
            {
                print!("{}", line_index + 1);
                for _ in 0..(index_width - (line_index + 1).ilog10()) {
                    print!(" ");
                }
                println!("| {}", line);
            }
            println!("{}", error.error);
            println!();
        }
        return;
    }

    interpreter::interpret(&statements, variables);
}

fn run_repl() {
    let mut variables = Variables::new();
    variables
        .environments
        .get_mut(&0)
        .unwrap()
        .variables
        .insert(
            ("print".to_string(), 0), // TODO: Prefer setting shadow_id somewhere else.
            Value::Function {
                parameters: vec![("value".to_string(), Some(0), Type::Any)],
                return_type: Type::Void,
                body: FunctionBody::RustClosure {
                    id: 0,
                    closure: Rc::new(|values| {
                        println!("{}", values[0]);
                        Value::Void
                    }),
                },
                parent_environment: 0, // Defined in global environment.
            },
        );
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        io::stdin().read_line(&mut line).unwrap();
        run(&line, &mut variables);
    }
}

fn run_file(filename: &str) {
    let Ok(program) = fs::read_to_string(filename) else {
        // TODO: Handle errors better - check if file doesn't exist.
        println!("Couldn't read the program.");
        return;
    };
    run(&program, &mut Variables::new());
}

fn main() {
    let mut args = env::args().skip(1);

    if args.len() > 1 {
        println!("Incorrect usage.");
        println!("To run REPL: busheye");
        println!("To run a file: busheye [filename]");
        return;
    }

    match args.next() {
        None => run_repl(),
        Some(filename) => run_file(&filename),
    }
}
