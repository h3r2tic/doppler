#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar); // synthesized by LALRPOP
extern crate regex;

mod ast;

use std::fs::File;
use std::io::prelude::*;
use regex::Regex;

fn main() {
    let mut f = File::open("linedraw.dr").expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

	// Remove comments
	let ws_re = Regex::new(r"//[^\n]*").unwrap();
	let contents = ws_re.replace_all(&contents, "");

	let ast = grammar::MainParser::new().parse(&contents).unwrap();

	println!("{:?}", ast);
		
}
