use std::collections::HashMap;
use ast;

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

pub fn const_fold_program(ast: &mut Vec<Box<ast::Stmt>>) {
    const_fold_stmt_list(
        ast,
        &mut ConstFoldScope {
            parent: None,
            slots: HashMap::new(),
        },
    );
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
			const_fold_expr(expr, ctx);
            if let Some(val) = get_const_folded_value(expr, ctx) {
                ctx.put(&ident.0, val);
            }
        }
        ast::Stmt::Var(_ident, expr) => {
            const_fold_expr(expr, ctx);
        }
        ast::Stmt::Loop(expr) => const_fold_expr(expr, ctx),
        ast::Stmt::For(_ident, _from, _to, expr) => const_fold_expr(expr, ctx),
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

fn get_const_folded_value<'a>(expr: &ast::Expr, ctx: &mut ConstFoldScope<'a>) -> Option<i32> {
	match expr {
		// Blocks need special treatment as they aren't immediately flattened
		ast::Expr::Block(_, res) => {
			if let Some(res) = res {
				get_const_folded_value(res, ctx)
			} else {
				panic!("Block const-folded to Empty, but a value is needed");
			}
		},
		ast::Expr::Value(value) => {
			match value {
				ast::Value::Int(val) => Some(*val),
				_ => panic!("Empty value encountered in const folding"),
			}
		},
		_ => None
	}
}

fn const_fold_expr<'a>(expr: &mut ast::Expr, ctx: &mut ConstFoldScope<'a>) {
    let folded : Option<ast::Expr> = match expr {
        ast::Expr::Block(stmt_list, ret_expr) => {
            let mut child_ctx = &mut ConstFoldScope {
                parent: Some(ctx),
                slots: HashMap::new(),
            };
            const_fold_stmt_list(stmt_list, &mut child_ctx);

			if let Some(ret_expr) = ret_expr {
				const_fold_expr(ret_expr, &mut child_ctx);
			}

			None
        }
        ast::Expr::If(cond, tex, fex) => {
			const_fold_expr(cond, ctx);
			if let Some(val) = get_const_folded_value(cond, ctx) {
				if val != 0 {
					const_fold_expr(tex, ctx);
					Some(ast::Expr::Block(
						vec![Box::new(ast::Stmt::Expr(cond.clone()))],
						Some(tex.clone())
					))
				} else if let Some(fex) = fex {
					const_fold_expr(fex, ctx);
					Some(ast::Expr::Block(
						vec![Box::new(ast::Stmt::Expr(cond.clone()))],
						Some(fex.clone())
					))
				} else {
					Some(ast::Expr::Value(ast::Value::Empty))
				}
			} else {
				None
			}
        }
        ast::Expr::Value(_) => None,
        ast::Expr::Name(name) => match name {
            ast::Name::Ident(ident) => ctx.get(&ident.0).map(|val| ast::Expr::Value(ast::Value::Int(val))),
            _ => None,
        },
        ast::Expr::Call(_, expr_list) => {
            const_fold_expr_list(expr_list, ctx);
            None
        }
        ast::Expr::Op(ref mut lhs, opcode, ref mut rhs) => {
            const_fold_expr(lhs, ctx);
            const_fold_expr(rhs, ctx);
            if let (ast::Expr::Value(ast::Value::Int(lhs)), ast::Expr::Value(ast::Value::Int(rhs))) = (&**lhs, &**rhs) {
                Some(ast::Expr::Value(ast::Value::Int(match opcode {
                    ast::Opcode::Mul => lhs * rhs,
                    ast::Opcode::Div => lhs / rhs,
                    ast::Opcode::Mod => lhs % rhs,
                    ast::Opcode::Add => lhs + rhs,
                    ast::Opcode::Sub => lhs - rhs,
                })))
            } else {
                None
            }
        }
        ast::Expr::Neg(expr) => {
			const_fold_expr(expr, ctx);
			if let ast::Expr::Value(ast::Value::Int(val)) = **expr {
				Some(ast::Expr::Value(ast::Value::Int(-val)))
			} else {
				None
			}
		},
    };

    if let Some(folded) = folded {
        *expr = folded;
    }
}
