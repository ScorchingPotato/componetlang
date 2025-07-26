use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::checker::Checker;
use std::path::Path;
use std::io::prelude::*;
use std::fs::File;


mod lexer;
mod parser;
mod checker;
mod standart_lib;

fn main() {
    let p = Path::new("main.txt");
    let mut file = match File::open(&p) {
        Err(why) => panic!("{:?}",why),
        Ok(file) => file
    };

    let mut code = String::new();
    match file.read_to_string(&mut code) {
        Err(why) => panic!("{:?}",why),
        Ok(_) => println!("Succesful read")
    }

    let mut l = Lexer::new(code);
    let tokens = l.tokenize().unwrap();

    let mut p = Parser::new(tokens);
    let program = match p.parse() {
        Ok(p) => Some(p),
        Err(e) => {println!("{}",e.throw());None}
    };
    if program.is_some() {
        println!("{:?}\n",program);
        let mut c= Checker::new(program.unwrap());
        match c.check() {
            Ok(p) => println!(""),
            Err(e) => println!("{:?}",e)
        }
    }
}
