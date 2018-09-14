use ast;
use il;

use std::collections::HashMap;

#[derive(Copy, Clone)]
struct TempVar(i32);

struct Scope(pub Vec<HashMap<String, TempVar>>);

impl Scope {
    fn new() -> Scope {
        Scope(vec![HashMap::new()])
    }

    fn push(&mut self) {
        self.0.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.0.pop();
    }

    fn get(&self, name: &str) -> Option<TempVar> {
        for slots in self.0.iter().rev() {
            if let Some(val) = slots.get(name) {
                return Some(*val);
            }
        }

        None
    }

    fn put(&mut self, name: &str, val: TempVar) {
        let first = self.0.len() - 1;
        self.0[first].insert(name.to_string(), val);
    }

    /*fn update(&mut self, name: &str, new_val: TempVar) {
        for slots in self.0.iter_mut().rev() {
            if slots.contains_key(name) {
                slots.insert(name.to_string(), new_val);
            }
        }
    }*/
}

pub fn ast_to_il(ast: &Vec<Box<ast::Stmt>>) -> Vec<il::Item> {
    let mut xlat = Translator::new();

    xlat.lower_stmt_list(ast, &mut Scope::new());
    xlat.il
}

struct Translator {
    pub il: Vec<il::Item>,
    pub next_temp: i32,
    pub next_label: i32,
}

impl Translator {
    fn new() -> Translator {
        Translator {
            il: vec![],
            next_temp: 0,
            next_label: 0,
        }
    }

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

    fn alloc_label(&mut self) -> i32 {
        let res = self.next_label;
        self.next_label += 1;
        res
    }

    /*fn emit_instr_with_temp_store<'a>(&mut self, instr: il::Instr, scope: &mut Scope) -> TempVar {
        let tmp =self.alloc_temp();
        //self.emit(il::Item::New(tmp));
        self.emit_instr(instr, tmp);
        //self.emit(il::Item::Store(il::Arg::Temp(tmp)));
        tmp
    }*/

    fn lower_stmt_list<'a>(&mut self, list: &Vec<Box<ast::Stmt>>, scope: &mut Scope) {
        for stmt in list.iter() {
            self.lower_stmt(&*stmt, scope);
        }
    }

    fn lower_let<'a>(&mut self, ident: &ast::Ident, expr: &ast::Expr, scope: &mut Scope) {
        let var = self.lower_expr(expr, scope).unwrap();
        if let il::Arg::Temp(idx) = var {
            scope.put(&ident.0, TempVar(idx));
        } else {
            let res = self.alloc_temp();

            self.emit_instr(il::Instr {
                op: il::Op::Copy,
                args: (var, il::Arg::None),
                res: il::Arg::Temp(res.0),
            });

            scope.put(&ident.0, res);
        }
    }

    fn lower_stmt<'a>(&mut self, stmt: &ast::Stmt, scope: &mut Scope) {
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
                let label = self.alloc_label();
                self.emit(il::Item::Label(label));
                self.lower_expr(expr, scope);
                self.emit(il::Item::Jump(label));
            }
            ast::Stmt::For(_ident, _from, _to, expr) => {
                // TODO
                self.lower_expr(expr, scope);
            }
            ast::Stmt::Assign(name, expr) => {
                let var = self.lower_expr(expr, scope).unwrap();
                match name {
                    ast::Name::Ident(ast::Ident(name)) => {
                        if let Some(prev_temp_idx) = scope.get(name) {
                            //let res = self.alloc_temp();
                            //scope.update(name, TempVar(res.0));

                            self.emit_instr(il::Instr {
                                op: il::Op::Copy,
                                args: (var, il::Arg::None),
                                res: il::Arg::Temp(prev_temp_idx.0),
                            });
                        } else {
                            panic!("Unrecognized identifier: {}", name);
                        }
                    }

                    ast::Name::Builtin(ast::Builtin(name)) => {
                        self.emit_instr(il::Instr {
                            op: il::Op::Copy,
                            args: (var, il::Arg::None),
                            res: il::Arg::Builtin(name.to_string()),
                        });
                    }
                }
            }
        }
    }

    /*fn const_fold_expr_list<'a>(&mut self, list: &Vec<Box<ast::Expr>>, scope: &mut Scope) {
        for expr in list.iter() {
            self.lower_expr(&*expr, scope);
        }
    }*/

    fn lower_if_else(
        &mut self,
        cond: &ast::Expr,
        tex: &ast::Expr,
        fex: &ast::Expr,
        scope: &mut Scope,
    ) -> Option<il::Arg> {
        let cond_var = self.lower_expr(cond, scope).unwrap();
        let result_reg = self.alloc_temp();

        let merge_label = self.alloc_label();
        let true_label = self.alloc_label();

        self.emit(il::Item::Tjmp(cond_var, true_label));

        let fex = self.lower_expr(fex, scope);
        if let Some(ref ret) = fex {
            self.emit_instr(il::Instr {
                op: il::Op::Copy,
                args: (ret.clone(), il::Arg::None),
                res: il::Arg::Temp(result_reg.0),
            });
        }
        self.emit(il::Item::Jump(merge_label));

        self.emit(il::Item::Label(true_label));

        let tex = self.lower_expr(tex, scope);
        if let Some(ref ret) = tex {
            self.emit_instr(il::Instr {
                op: il::Op::Copy,
                args: (ret.clone(), il::Arg::None),
                res: il::Arg::Temp(result_reg.0),
            });
        }

        self.emit(il::Item::Label(merge_label));

        if tex.is_some() {
            Some(il::Arg::Temp(result_reg.0))
        } else {
            None
        }
    }

    fn lower_if(
        &mut self,
        cond: &ast::Expr,
        tex: &ast::Expr,
        scope: &mut Scope,
    ) -> Option<il::Arg> {
        let cond_var = self.lower_expr(cond, scope).unwrap();
        let false_label = self.alloc_label();

        self.emit(il::Item::Fjmp(cond_var, false_label));
        self.lower_expr(tex, scope);
        self.emit(il::Item::Label(false_label));
        None
    }

    fn lower_expr<'a>(&mut self, expr: &ast::Expr, scope: &mut Scope) -> Option<il::Arg> {
        match expr {
            ast::Expr::Block(stmt_list, res) => {
                scope.push();
                self.lower_stmt_list(stmt_list, scope);

                let ret = if let Some(res) = res {
                    self.lower_expr(res, scope)
                } else {
                    None
                };

                scope.pop();
                ret
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
            ast::Expr::Call(_ident, _args) => {
				Some(il::Arg::Builtin("TODO:call".to_string()))
			},
            ast::Expr::Op(lhs, op, rhs) => {
                let lhs = self.lower_expr(lhs, scope).unwrap();
                let rhs = self.lower_expr(rhs, scope).unwrap();

                let res = self.alloc_temp();

                self.emit_instr(il::Instr {
                    op: match op {
                        ast::Opcode::Mul => il::Op::Mul,
                        ast::Opcode::Div => il::Op::Div,
                        ast::Opcode::Mod => il::Op::Mod,
                        ast::Opcode::Add => il::Op::Add,
                        ast::Opcode::Sub => il::Op::Sub,
                    },
                    args: (lhs, rhs),
                    res: il::Arg::Temp(res.0),
                });

                Some(il::Arg::Temp(res.0))
            }
            ast::Expr::Neg(expr) => {
                let val = self.lower_expr(expr, scope).unwrap();

                let res = self.alloc_temp();

                self.emit_instr(il::Instr {
                    op: il::Op::Sub,
                    args: (il::Arg::Const(0), val),
                    res: il::Arg::Temp(res.0),
                });

                Some(il::Arg::Temp(res.0))
            }
            ast::Expr::If(cond, tex, fex) => {
                if let Some(ref fex) = fex {
                    self.lower_if_else(cond, tex, fex, scope)
                } else {
                    self.lower_if(cond, tex, scope);
                    None
                }
            }
        }
    }
}
