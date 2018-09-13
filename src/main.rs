#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub grammar); // synthesized by LALRPOP
extern crate regex;

mod ast;

use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

struct ConstFoldScope<'a> {
    pub parent: Option<&'a ConstFoldScope<'a>>,
    pub slots: HashMap<String, i32>,
}

impl<'a> ConstFoldScope<'a> {
    fn get(&self, name: &str) -> Option<i32> {
        if let Some(val) = self.slots.get(name) {
            Some(*val)
        } else {
            self.parent.map(|p| p.get(name)).unwrap_or_default()
        }
    }

    fn put(&mut self, name: &str, val: i32) {
        self.slots.insert(name.to_string(), val);
    }
}

fn const_fold_stmt_list<'a>(list: &mut Vec<Box<ast::Stmt>>, ctx: &mut ConstFoldScope<'a>) {
    for stmt in list.iter_mut() {
        const_fold_stmt(&mut *stmt, ctx);
    }
}

fn const_fold_stmt<'a>(stmt: &mut ast::Stmt, ctx: &mut ConstFoldScope<'a>) {
    match stmt {
        ast::Stmt::Expr(expr) => {
            const_fold_expr(expr, ctx);
        }
        ast::Stmt::Let(ident, expr) => {
            if let Some(val) = const_fold_expr(expr, ctx) {
                ctx.put(&ident.0, val);
            }
        }
        ast::Stmt::Var(_ident, expr) => {
            const_fold_expr(expr, ctx);
        }
        ast::Stmt::Block(stmt_list) => {
            let mut child_ctx = &mut ConstFoldScope {
                parent: Some(ctx),
                slots: HashMap::new(),
            };
            const_fold_stmt_list(stmt_list, &mut child_ctx);
        }
        ast::Stmt::Loop(stmt) => const_fold_stmt(stmt, ctx),
        ast::Stmt::For(_ident, _from, _to, stmt) => const_fold_stmt(stmt, ctx),
        ast::Stmt::Assign(_name, expr) => {
            const_fold_expr(expr, ctx);
        }
    }
}

fn const_fold_expr_list<'a>(list: &mut Vec<Box<ast::Expr>>, ctx: &mut ConstFoldScope<'a>) {
    for expr in list.iter_mut() {
        const_fold_expr(&mut *expr, ctx);
    }
}

fn const_fold_expr<'a>(expr: &mut ast::Expr, ctx: &mut ConstFoldScope<'a>) -> Option<i32> {
    let result = match expr {
        ast::Expr::Number(val) => Some(*val),
        ast::Expr::Name(name) => match name {
            ast::Name::Ident(ident) => ctx.get(&ident.0),
            _ => None,
        },
        ast::Expr::Call(_, expr_list) => {
            const_fold_expr_list(expr_list, ctx);
            None
        }
        ast::Expr::Op(lhs, opcode, rhs) => {
            let lhs = const_fold_expr(lhs, ctx);
            let rhs = const_fold_expr(rhs, ctx);
            if let (Some(lhs), Some(rhs)) = (lhs, rhs) {
                Some(match opcode {
                    ast::Opcode::Mul => lhs * rhs,
                    ast::Opcode::Div => lhs / rhs,
                    ast::Opcode::Mod => lhs % rhs,
                    ast::Opcode::Add => lhs + rhs,
                    ast::Opcode::Sub => lhs - rhs,
                })
            } else {
                None
            }
        }
        ast::Expr::Neg(expr) => const_fold_expr(expr, ctx).map(|val| -val),
    };

    if let Some(val) = result {
        *expr = ast::Expr::Number(val);
    }

    result
}

fn main() {
    let mut f = File::open("linedraw.dr").expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    // Remove comments
    let ws_re = Regex::new(r"//[^\n]*").unwrap();
    let contents = ws_re.replace_all(&contents, "");

    //let contents = "let x = 2 + 2 * 2; let y = x % 4;";

    let mut ast = grammar::MainParser::new().parse(&contents).unwrap();

    println!("{:?}", ast);
    const_fold_stmt_list(
        &mut ast,
        &mut ConstFoldScope {
            parent: None,
            slots: HashMap::new(),
        },
    );
    println!("\nconst folded:\n");
    println!("{:?}", ast);
}
