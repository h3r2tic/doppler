#[derive(Debug)]
pub enum Stmt {
    Expr(Box<Expr>),
    Let(Ident, Box<Expr>),
    Var(Ident, Box<Expr>),
    Block(Vec<Box<Stmt>>),
    Loop(Box<Stmt>),
    For(Ident, i32, i32, Box<Stmt>),
    Assign(Name, Box<Expr>),
}

#[derive(Debug)]
pub struct Ident(pub String);

#[derive(Debug)]
pub struct Builtin(pub String);

#[derive(Debug)]
pub enum Name {
    Ident(Ident),
    Builtin(Builtin),
}

#[derive(Debug)]
pub enum Expr {
    Number(i32),
    Name(Name),
    Call(Ident, Vec<Box<Expr>>),
    Op(Box<Expr>, Opcode, Box<Expr>),
    Neg(Box<Expr>),
}

#[derive(Debug)]
pub enum Opcode {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
}
