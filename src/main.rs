use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::checker::Checker;
use crate::standart_lib::get_type_by_op;
use std::path::Path;
use std::io::prelude::*;
use std::fs::File;
use inkwell::context::Context;

mod lexer;
mod parser;
mod checker;
mod standart_lib;
mod compiler;

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
        let mut c= Checker::new(program.unwrap());
        match c.raw() {
            Ok(p) => println!(""),
            Err(e) => e.throw(),
        }
        let cmpl = compiler::Compiler {
            fields: c.fields,
            glob_funcs: c.functions,
            enums: c.enums,
            context: Context::create(), 
        };
        cmpl.run()
    }
}
