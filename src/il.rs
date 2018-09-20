use std::fmt;

#[derive(Clone, Debug)]
pub enum Arg {
    Temp(i32),
    Builtin(String),
    //Stack(i32),
    Const(i32),
    None,
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Arg::Temp(i) => write!(f, "t{}", i),
            Arg::Builtin(s) => write!(f, "@{}", s),
            //Arg::Stack(i) => write!(f, "f{}", i),
            Arg::Const(i) => write!(f, "{}", i),
            Arg::None => f.write_str(""),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Copy,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Op::Add => "add",
            Op::Sub => "sub",
            Op::Mul => "mul",
            Op::Div => "div",
            Op::Mod => "mod",
            Op::Copy => "copy",
        })
    }
}

#[derive(Clone, Debug)]
pub struct Instr {
    pub op: Op,
    pub args: (Arg, Arg),
    pub res: Arg,
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} = {} {} {}",
            self.res, self.op, self.args.0, self.args.1
        )
    }
}

#[derive(Clone, Debug)]
pub enum JumpTarget {
    Block(usize),
    Label(i32),
}

impl fmt::Display for JumpTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            JumpTarget::Label(i) => write!(f, "label{}", i),
            JumpTarget::Block(i) => write!(f, "block{}", i),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Item {
    Label(i32),
    Jump(JumpTarget),
    Fjmp(Arg, JumpTarget),
    Tjmp(Arg, JumpTarget),
    Instr(Instr),
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Item::Label(i) => write!(f, "label{}:", i),
            Item::Jump(i) => write!(f, "jump {}", i),
            Item::Fjmp(arg, i) => write!(f, "fjmp({}) label{}", arg, i),
            Item::Tjmp(arg, i) => write!(f, "tjmp({}) label{}", arg, i),
            Item::Instr(i) => write!(f, "{}", i),
        }
    }
}
