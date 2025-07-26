use std::collections::HashMap;
use crate::parser::{Visibility,BinaryOp};
use crate::checker::{Func,Type,Var,Value};

pub fn bool_op() -> [BinaryOp;4] {
    [BinaryOp::Eq,BinaryOp::And,BinaryOp::Or,BinaryOp::Ne]
}
pub fn comp_op() -> [BinaryOp;6] {
    [BinaryOp::Eq,BinaryOp::Ne,BinaryOp::Gt,BinaryOp::Gte,BinaryOp::Lt,BinaryOp::Lte]
}
pub fn num_op() -> Vec<BinaryOp> {
    let mut v = Vec::new();
    v.extend_from_slice(&[BinaryOp::Add,BinaryOp::Sub,BinaryOp::Mul,BinaryOp::Div,BinaryOp::Mod]);
    v.extend_from_slice(&comp_op());
    v
}
pub fn int_float_op() -> [BinaryOp;5] {
    [BinaryOp::Add,BinaryOp::Sub,BinaryOp::Mul,BinaryOp::Div,BinaryOp::Mod]
}
pub fn str_op() -> [BinaryOp;3] {
    [BinaryOp::Add,BinaryOp::Eq,BinaryOp::Ne]
}
pub fn str_int_op() -> [BinaryOp;1] {
    [BinaryOp::Mul]
}

pub fn global_functions() -> HashMap<String,Func> {
    let mut fs = HashMap::new();
    fs.insert("print".to_string(), 
        Func {
            vis: Visibility::Public,
            ret: Type::Null,
            args: {let mut a = HashMap::new(); a.insert("out".to_string(),Var {var_type:Type::Str,value:Value::Empty});a},
            vars: HashMap::new(),
            body: Vec::new()
        }
    );
    fs
}