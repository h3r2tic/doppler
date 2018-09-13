#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Box<Expr>),
    Let(Ident, Box<Expr>),
    Var(Ident, Box<Expr>),
    Loop(Box<Stmt>),
    For(Ident, i32, i32, Box<Stmt>),
    Assign(Name, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Ident(pub String);

#[derive(Debug, Clone)]
pub struct Builtin(pub String);

#[derive(Debug, Clone)]
pub enum Name {
    Ident(Ident),
    Builtin(Builtin),
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Int(i32),
    Empty,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Block(Vec<Box<Stmt>>, Option<Box<Expr>>),
    Value(Value),
    Name(Name),
    Call(Ident, Vec<Box<Expr>>),
    Op(Box<Expr>, Opcode, Box<Expr>),
    Neg(Box<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
}

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
}
