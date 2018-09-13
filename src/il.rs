pub enum Operand {
    Temp(i32),
    Reg(i32),
    Stack(i32),
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

pub struct BinaryInstr {
    lhs: Operand,
    rhs: Operand,
    result: Operand,
    op: BinaryOp,
}

pub enum Instr {
    BinaryInstr(BinaryInstr),
}

pub enum Item {
    New(i32),
    Drop(i32),
    Label(i32),
    Instr(Instr),
}