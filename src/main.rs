use std::{
    env, fs,
    io::{self, Write},
};

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => run_prompt(),
        2 => run_file(args.get(1).unwrap()),
        _ => panic!("Usage: jlox [script]"),
    }
}

fn run_prompt() {
    let stdin = io::stdin();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        match stdin.read_line(&mut line) {
            Ok(0) => break,
            Ok(_) => run(line),
            Err(err) => panic!("Error: {}", err),
        }
    }
}

fn run_file(file_name: &str) {
    let content = fs::read_to_string(file_name).expect("Error: Wrong path");
    run(content);
}

fn run(content: String) {
    println!("Run: {}", content);
}
