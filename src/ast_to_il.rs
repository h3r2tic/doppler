use ast;
use il;

use std::collections::HashMap;

#[derive(Copy, Clone)]
struct TempVar(i32);

struct Scope<'a> {
    pub parent: Option<&'a Scope<'a>>,
    pub slots: HashMap<String, TempVar>,
}

impl<'a> Scope<'a> {
    fn get(&self, name: &str) -> Option<TempVar> {
        if let Some(val) = self.slots.get(name) {
            Some(*val)
        } else {
            self.parent.map(|p| p.get(name)).unwrap_or_default()
        }
    }

    fn put(&mut self, name: &str, val: TempVar) {
        self.slots.insert(name.to_string(), val);
    }
}

pub fn ast_to_il(ast: &Vec<Box<ast::Stmt>>) -> Vec<il::Item> {
    let mut xlat = Translator {
        il: vec![],
        next_temp: 0,
    };

    xlat.lower_stmt_list(
        ast,
        &mut Scope {
            parent: None,
            slots: HashMap::new(),
        },
    );
    xlat.il
}

struct Translator {
    pub il: Vec<il::Item>,
    pub next_temp: i32,
}

impl Translator {
    fn emit(&mut self, item: il::Item) {
        self.il.push(item);
    }

    fn emit_instr(&mut self, instr: il::Instr) {
        self.il.push(il::Item::Instr(instr));
    }

    fn alloc_temp(&mut self) -> TempVar {
        let res = self.next_temp;
        self.next_temp += 1;
        TempVar(res)
    }

    fn emit_instr_with_temp_store<'a>(&mut self, instr: il::Instr, scope: &mut Scope<'a>) -> TempVar {
        let tmp =self.alloc_temp().0;
        self.emit(il::Item::New(tmp));
        self.emit_instr(instr);
        self.emit(il::Item::Store(il::Arg::Temp(tmp)));
        TempVar(tmp)
    }

    fn lower_stmt_list<'a>(&mut self, list: &Vec<Box<ast::Stmt>>, scope: &mut Scope<'a>) {
        for stmt in list.iter() {
            self.lower_stmt(&*stmt, scope);
        }
    }

    fn lower_let<'a>(&mut self, ident: &ast::Ident, expr: &ast::Expr, scope: &mut Scope<'a>) {
        let var = self.lower_expr(expr, scope).unwrap();
        if let il::Arg::Temp(idx) = var {
            scope.put(&ident.0, TempVar(idx));
        } else {
            let res = self.emit_instr_with_temp_store(il::Instr {
                op: il::Op::Mov,
                args: (var, il::Arg::None),
            }, scope);

            scope.put(&ident.0, res);
        }
    }

    fn lower_stmt<'a>(&mut self, stmt: &ast::Stmt, scope: &mut Scope<'a>) {
        match stmt {
            ast::Stmt::Expr(expr) => {
                self.lower_expr(expr, scope);
            }
            ast::Stmt::Let(ident, expr) => {
                self.lower_let(ident, expr, scope);
            }
            ast::Stmt::Var(ident, expr) => {
                self.lower_let(ident, expr, scope);
            }
            ast::Stmt::Loop(expr) => {
                // TODO
                self.lower_expr(expr, scope);
            }
            ast::Stmt::For(ident, from, to, expr) => {
                // TODO
                self.lower_expr(expr, scope);
            }
            ast::Stmt::Assign(name, expr) => {
                // TODO
                self.lower_expr(expr, scope);
            }
        }
    }

    fn const_fold_expr_list<'a>(&mut self, list: &Vec<Box<ast::Expr>>, scope: &mut Scope<'a>) {
        for expr in list.iter() {
            self.lower_expr(&*expr, scope);
        }
    }

    fn lower_expr<'a>(&mut self, expr: &ast::Expr, scope: &mut Scope<'a>) -> Option<il::Arg> {
        match expr {
            ast::Expr::Block(stmt_list, res) => {
                let mut child_scope = &mut Scope {
                    parent: Some(scope),
                    slots: HashMap::new(),
                };

                self.lower_stmt_list(stmt_list, &mut child_scope);

                if let Some(res) = res {
                    self.lower_expr(res, &mut child_scope)
                } else {
                    None
                }
            }
            ast::Expr::Value(value) => {
                if let ast::Value::Int(value) = value {
                    Some(il::Arg::Const(*value))
                } else {
                    panic!("Empty type encountered while lowering to IL");
                }
            }
            ast::Expr::Name(name) => match name {
                ast::Name::Ident(ident) => {
                    if let Some(var) = scope.get(&ident.0) {
                        Some(il::Arg::Temp(var.0))
                    } else {
                        panic!("Unrecognized identifier {}", ident.0);
                    }
                }
                ast::Name::Builtin(reg) => Some(il::Arg::Builtin(reg.0.clone())),
            },
            ast::Expr::Call(_ident, _args) => None, // TODO
            ast::Expr::Op(lhs, op, rhs) => {
                let lhs = self.lower_expr(lhs, scope).unwrap();
                let rhs = self.lower_expr(rhs, scope).unwrap();

                let res = self.emit_instr_with_temp_store(il::Instr {
                    op: match op {
                        ast::Opcode::Mul => il::Op::Mul,
                        ast::Opcode::Div => il::Op::Div,
                        ast::Opcode::Mod => il::Op::Mod,
                        ast::Opcode::Add => il::Op::Add,
                        ast::Opcode::Sub => il::Op::Sub,
                    },
                    args: (lhs, rhs),
                }, scope);

                Some(il::Arg::Temp(res.0))
            }
            ast::Expr::Neg(expr) => {
                let val = self.lower_expr(expr, scope).unwrap();

                let tmp =self.alloc_temp().0;
                self.emit(il::Item::New(tmp));

                self.emit_instr(il::Instr {
                    op: il::Op::Sub,
                    args: (il::Arg::Const(0), val),
                });
                let res = il::Arg::Temp(tmp);
                self.emit(il::Item::Store(res.clone()));
                Some(res)
            }
            ast::Expr::If(cond, tex, fex) => None,
        }
    }
}
