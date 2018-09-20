#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub grammar); // synthesized by LALRPOP
extern crate regex;

mod ast;
mod ast_to_il;
mod const_fold;
mod il;
mod resolve_names;

use ast_to_il::ast_to_il;
use const_fold::const_fold_program;
use resolve_names::resolve_names;

use regex::Regex;
use std::fs::File;
use std::io::prelude::*;


fn main() {
    let mut f = File::open("test/simple6.dr").expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    // Remove comments
    let ws_re = Regex::new(r"//[^\n]*").unwrap();
    let contents = ws_re.replace_all(&contents, "");

    //let contents = "let x = 2 + 2 * 2; let y = x % 4;";

    let mut ast = grammar::MainParser::new().parse(&contents).unwrap();
    //println!("{:?}", ast);
    const_fold_program(&mut ast);
	resolve_names(&mut ast);
    //println!("\nconst folded:\n");
    println!("ast: {:?}", ast);
    //println!("\nil:");

    /*let il = */ast_to_il(&ast);
    /*for item in il.iter() {
        println!("{}", item);
    }*/
}
