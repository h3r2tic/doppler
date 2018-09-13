#[derive(Clone, Debug)]
pub enum Arg {
    Temp(i32),
    Builtin(String),
    Stack(i32),
    Const(i32),
    None,
}

#[derive(Copy, Clone, Debug)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Mov,
}

#[derive(Clone, Debug)]
pub struct Instr {
    pub op: Op,
    pub args: (Arg, Arg),
}

#[derive(Clone, Debug)]
pub enum Item {
    New(i32),
    Drop(i32),
    Label(i32),
    Instr(Instr),
    Store(Arg),
}
