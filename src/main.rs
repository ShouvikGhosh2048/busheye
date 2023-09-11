mod interpreter;
mod parser;
mod tokenizer;
mod variable_and_type_check;

use std::{
    collections::HashMap,
    env, fs,
    io::{self, Write},
};

use tokenizer::Value;
use variable_and_type_check::check_types;

fn run(program: &str, global_variables: &mut HashMap<String, Value>) {
    let tokens = match tokenizer::tokenize(program) {
        Ok(tokens) => tokens,
        Err(errors) => {
            for error in errors {
                println!("{error}");
            }
            return;
        }
    };

    let mut global_types = HashMap::new();
    for (variable, value) in global_variables.iter() {
        global_types.insert(variable.clone(), value.value_type());
    }
    let statements = match parser::parse(&tokens) {
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

    if let Err(errors) = check_types(&statements, &global_types) {
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

    interpreter::interpret(&statements, global_variables);
}

fn run_repl() {
    let mut global_variables = HashMap::new();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        io::stdin().read_line(&mut line).unwrap();
        run(&line, &mut global_variables);
    }
}

fn run_file(filename: &str) {
    let Ok(program) = fs::read_to_string(filename) else {
        // TODO: Handle errors better - check if file doesn't exist.
        println!("Couldn't read the program.");
        return;
    };
    run(&program, &mut HashMap::new());
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
