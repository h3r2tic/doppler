use ast;
use il;

use std::collections::HashMap;

#[derive(Copy, Clone)]
struct TempVar(i32);

/*struct Scope(pub Vec<HashMap<String, TempVar>>);

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
}*/

pub fn ast_to_il(ast: &Vec<Box<ast::Stmt>>) -> Vec<il::Item> {
    let mut xlat = Translator::new();

    xlat.lower_stmt_list(ast);
    //xlat.il

    for (i, block) in xlat.blocks.iter().enumerate() {
        println!("block{}", i);
        for item in block.il.iter() {
            println!("{}", item);
        }
    }

    // TODO
    vec![]
}

#[derive(Default)]
struct Block {
    pub il: Vec<il::Item>,
    pub pred: Vec<usize>,
    pub succ: Vec<usize>,
	pub vars: HashMap<String, TempVar>,
}

struct Translator {
    pub blocks: Vec<Block>,
    pub next_temp: i32,
    pub next_label: i32,
}

impl Translator {
    fn new() -> Translator {
        Translator {
            blocks: vec![Block::default()],
            next_temp: 0,
            next_label: 0,
        }
    }

    fn weave_block(&mut self, pred: Vec<usize>) -> usize {
        let ret = self.blocks.len();
		//println!("allocating new block {}; parent: {}", ret, pred);
        self.blocks.push(Block::default());

		for p in pred.iter() {
        	self.blocks[*p].succ.push(ret);
		}

        self.blocks[ret].pred = pred;
        ret
    }

    fn chain_block(&mut self) -> usize {
        let ret = self.blocks.len();
		let pred = ret - 1;
		//println!("allocating new block {}; parent: {}", ret, pred);
        self.blocks.push(Block::default());
       	self.blocks[pred].succ.push(ret);
        self.blocks[ret].pred.push(pred);
        ret
    }

    fn emit(&mut self, block: usize, item: il::Item) {
        self.blocks[block].il.push(item);
    }

    fn emit_instr(&mut self, block: usize, instr: il::Instr) {
        self.blocks[block].il.push(il::Item::Instr(instr));
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

    fn lower_stmt_list<'a>(&mut self, list: &Vec<Box<ast::Stmt>>) {
        for stmt in list.iter() {
            self.lower_stmt(&*stmt);
        }
    }

    fn lower_let<'a>(&mut self, ident: &ast::Ident, expr: &ast::Expr, block: usize) {
        let var = self.lower_expr(expr, block).unwrap();

        if let il::Arg::Temp(idx) = var {
			//println!("adding {} to block {}", ident.0, block);
            self.blocks[block].vars.insert(ident.0.to_string(), TempVar(idx));
        } else {
            let res = self.alloc_temp();

            self.emit_instr(block, il::Instr {
                op: il::Op::Copy,
                args: (var, il::Arg::None),
                res: il::Arg::Temp(res.0),
            });

			//println!("adding {} to block {}", ident.0, block);
            self.blocks[block].vars.insert(ident.0.to_string(), res);
        }
    }

    fn lower_stm(&mut self, stmt: &ast::Stmt) -> usize {
        let block = self.chain_block();

        match stmt {
            ast::Stmt::Expr(expr) => {
                self.lower_expr(expr, block);
            }
            ast::Stmt::Let(ident, expr) => {
                self.lower_let(ident, expr, block);
            }
            ast::Stmt::Var(ident, expr) => {
                self.lower_let(ident, expr, block);
            }
            ast::Stmt::Loop(expr) => {
                let label = self.alloc_label();
                //self.emit(il::Item::Label(label));
                self.lower_expr(expr, block);
                //self.emit(il::Item::Jump(label));
            }
            ast::Stmt::For(_ident, _from, _to, expr) => {
                // TODO
                self.lower_expr(expr, block);
            }
            ast::Stmt::Assign(name, expr) => {
                let var = self.lower_expr(expr, block).unwrap();
                /*match name {
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
                }*/
            }
        }

		block
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
        parent_block: usize,
    ) -> Option<il::Arg> {
        let cond_var = self.lower_expr(cond, scope).unwrap();
        let result_reg = self.alloc_temp();

		let parent_block = self.blocks.len() - 1;
		let true_block = self.weave_block(vec![parent_block]);
		let false_block = self.weave_block(vec![parent_block]);
		let merge_block = self.weave_block(vec![true_block, false_block]);

        /*let merge_label = self.alloc_label();
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
        }*/

	// TODO
		None
    }

    fn lower_if(
        &mut self,
        cond: &ast::Expr,
        tex: &ast::Expr,
        parent_block: usize,
    ) -> Option<il::Arg> {
        /*let cond_var = self.lower_expr(cond, scope).unwrap();
        let false_label = self.alloc_label();

        self.emit(il::Item::Fjmp(cond_var, false_label));
        self.lower_expr(tex, scope);
        self.emit(il::Item::Label(false_label));*/

		// TODO
        None
    }

	fn resolve_var(&mut self, block: usize, name: &str) -> Option<TempVar> {
		//println!("+resolve_var block:{} name:{}", block, name);

		{
			let b = &self.blocks[block];
			if b.vars.contains_key(name) {
				//println!("-block contains name; returning");
				return Some(b.vars[name]);
			}
		}
		
		let pred_defs = self.blocks[block].pred.clone();
		//println!("{} predecessors", pred_defs.len());

		let pred_defs : Vec<TempVar> = pred_defs.iter().filter_map(|pred| {
			self.resolve_var(*pred, name)
		}).collect();

		//println!("-{} names found", pred_defs.len());

		if 1 == pred_defs.len() {
			Some(pred_defs[0])
		} else {
			// TODO: phi
			None
		}
	}

    fn lower_expr<'a>(&mut self, expr: &ast::Expr) -> Option<il::Arg> {
        match expr {
            ast::Expr::Block(stmt_list, res) => {
                let block = self.chain_block();
                self.lower_stmt_list(stmt_list);

                let ret = if let Some(res) = res {
                    self.lower_expr(res, block)
                } else {
                    None
                };

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
					if let Some(var) = self.resolve_var(parent_block, &ident.0) {
						Some(il::Arg::Temp(var.0))
					} else {
						panic!("Unrecognized identifier {}", &ident.0);
					}
                },
               	ast::Name::Builtin(reg) => Some(il::Arg::Builtin(reg.0.clone()))
            },
            ast::Expr::Call(_ident, _args) => {
				Some(il::Arg::Builtin("TODO:call".to_string()))
			},
            ast::Expr::Op(lhs, op, rhs) => {
                let lhs = self.lower_expr(lhs, parent_block).unwrap();
                let rhs = self.lower_expr(rhs, parent_block).unwrap();

                let res = self.alloc_temp();

                self.emit_instr(parent_block, il::Instr {
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
                let val = self.lower_expr(expr, parent_block).unwrap();
                let res = self.alloc_temp();

                self.emit_instr(parent_block, il::Instr {
                    op: il::Op::Sub,
                    args: (il::Arg::Const(0), val),
                    res: il::Arg::Temp(res.0),
                });

                Some(il::Arg::Temp(res.0))
            }
            ast::Expr::If(cond, tex, fex) => {
                if let Some(ref fex) = fex {
                    self.lower_if_else(cond, tex, fex, parent_block)
                } else {
                    self.lower_if(cond, tex, parent_block);
                    None
                }
            }
        }
    }
}
