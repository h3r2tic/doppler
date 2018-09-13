pub enum Arg {
    Temp(i32),
    Reg(i32),
    Stack(i32),
    None
}

pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Mov,
}

pub struct Instr {
    op: Op,
    args: (Arg, Arg),
    res: Arg,
}

pub enum Item {
    New(i32),
    Drop(i32),
    Label(i32),
    Instr(Instr),
}