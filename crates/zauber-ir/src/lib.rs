//! An intermediate representation for file classification programs

/*
pub struct Block {
    pub instructions: Vec<Instr>,
}
*/

pub enum MatchExpr {
    /// Short-circuiting conjunction of expressions
    All(Vec<MatchExpr>),

    /// Short-circuiting disjunction of expressions
    Any(Vec<MatchExpr>),

    Lit(Literal)
}

pub enum Literal {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),

    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),

    F32(f32),
    F64(f64),
}
