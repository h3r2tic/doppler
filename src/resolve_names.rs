use ast;

use std::collections::HashMap;

pub fn resolve_names(ast: &mut Vec<Box<ast::Stmt>>) {
    resolve_stmt_list(
        ast,
        &mut Scope::new(),
    );
}

struct Scope {
	pub next_decor: u32,
	pub blocks: Vec<HashMap<String, String>>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
			next_decor: 0,
			blocks: vec![HashMap::new()]
		}
    }

    fn push(&mut self) {
        self.blocks.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.blocks.pop();
    }

	fn decorate_name(&mut self, name: &str) -> String {
		let decor = self.next_decor;
		self.next_decor += 1;
		format!("{}@{}", name, decor)
	}

    fn get(&self, name: &str) -> Option<String> {
        for slots in self.blocks.iter().rev() {
            if let Some(val) = slots.get(name) {
                return Some(val.clone());
            }
        }

        None
    }

    fn put(&mut self, name: &str, val: String) {
        let first = self.blocks.len() - 1;
        self.blocks[first].insert(name.to_string(), val);
    }
}

fn resolve_stmt_list(list: &mut Vec<Box<ast::Stmt>>, scope: &mut Scope) {
    for stmt in list.iter_mut() {
        resolve_stmt(&mut *stmt, scope);
    }
}

fn resolve_name(name: &mut ast::Name, scope: &mut Scope) {
	let new_name = match name {
		ast::Name::Ident(ident) => {
			match scope.get(&ident.0) {
				Some(new_name) => {
					Some(ast::Name::Ident(ast::Ident(new_name)))
				},
				None => panic!("Unrecognized identifier {}", ident.0),
			}
		}
		_ => None
	};

	if let Some(new_name) = new_name {
		*name = new_name;
	}
}

fn resolve_stmt(stmt: &mut ast::Stmt, scope: &mut Scope) {
    let resolved = match stmt {
        ast::Stmt::Expr(expr) => {
            resolve_expr(expr, scope);
        }
        ast::Stmt::Let(ident, expr) => {
            resolve_expr(expr, scope);
			let new_name = scope.decorate_name(&ident.0);
            scope.put(&ident.0, new_name.clone());
			*ident = ast::Ident(new_name);
        },
        ast::Stmt::Var(ident, expr) => {
            resolve_expr(expr, scope);
			let new_name = scope.decorate_name(&ident.0);
            scope.put(&ident.0, new_name.clone());
			*ident = ast::Ident(new_name);
		},
        ast::Stmt::Loop(expr) => resolve_expr(expr, scope),
        ast::Stmt::For(_ident, _from, _to, expr) => resolve_expr(expr, scope),
        ast::Stmt::Assign(name, expr) => {
			resolve_expr(expr, scope);
			resolve_name(name, scope);
		}
    };
}

fn resolve_expr_list(list: &mut Vec<Box<ast::Expr>>, scope: &mut Scope) {
    for expr in list.iter_mut() {
        resolve_expr(&mut *expr, scope);
    }
}

fn resolve_expr(expr: &mut ast::Expr, scope: &mut Scope) {
    match expr {
        ast::Expr::Block(stmt_list, ret_expr) => {
			scope.push();
            resolve_stmt_list(stmt_list, scope);

            if let Some(ret_expr) = ret_expr {
                resolve_expr(ret_expr, scope);
            }
			scope.pop();
        }
        ast::Expr::If(cond, tex, fex) => {
            resolve_expr(cond, scope);
			resolve_expr(tex, scope);
			if let Some(fex) = fex {
				resolve_expr(fex, scope);
			}
        }
        ast::Expr::Value(_) => (),
        ast::Expr::Name(name) => resolve_name(name, scope),
        ast::Expr::Call(_, expr_list) => resolve_expr_list(expr_list, scope),
        ast::Expr::Op(ref mut lhs, opcode, ref mut rhs) => {
            resolve_expr(lhs, scope);
            resolve_expr(rhs, scope);
        }
        ast::Expr::Neg(expr) => resolve_expr(expr, scope),
    };
}
