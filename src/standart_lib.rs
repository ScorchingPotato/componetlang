use std::collections::HashMap;
use crate::parser::{Visibility,BinaryOp};
use crate::checker::{CheckerError, Comp, Field, Func, Type, Value, Var};

pub fn comp_op() -> [BinaryOp;6] {
    [BinaryOp::Eq,BinaryOp::Ne,BinaryOp::Gt,BinaryOp::Gte,BinaryOp::Lt,BinaryOp::Lte]
}

fn is_num(t:&Type) -> bool {
    [Type::Int,Type::Float].contains(t)
}

fn can_add(l:&Type,r:&Type) -> bool {
    if l==r {
        match l {
            Type::Bool => false,
            Type::Null => false,
            Type::Option(tps) => tps.iter().map(|t| can_add(t,r)).all(|t| t),
            Type::Custom(_) => false,
            _ => true
        }
    } else {
        is_num(l) && is_num(r)
    }
}
fn can_sub(l:&Type,r:&Type) -> bool {
    if l==r {
        match l {
            Type::Bool => false,
            Type::Null => false,
            Type::Str => false,
            Type::Option(tps) => tps.iter().map(|t| can_sub(t,r)).all(|t| t),
            Type::Custom(_) => false,
            Type::Arr(_) => false,
            _ => true
        }
    } else {
        is_num(l) && is_num(r)
    }
}
fn can_mult(l:&Type,r:&Type) -> bool {
    match r {
        Type::Int => match l {
            Type::Int => true,
            Type::Float => true,
            Type::Str => true,
            Type::Arr(_) => true,
            Type::Option(tps) => tps.iter().map(|t| can_mult(t,r)).all(|t| t),
            _ => false
        }
        Type::Float => is_num(l),
        Type::Option(tps) => tps.iter().map(|t| can_mult(l,t)).all(|t| t),
        _ => false
    }
}

pub fn comp(left:&Type,right:&Type) -> bool {
    left==right
}
pub fn bit(left:&Type,right:&Type) -> bool {
    left==&Type::Bool && right==&Type::Bool
}
pub fn add(left:&Type,right:&Type) -> bool {
    can_add(left,right)
}
pub fn sub(left:&Type,right:&Type) -> bool {
    can_sub(left,right)
}
pub fn mul(left:&Type,right:&Type) -> bool {
    can_mult(left, right)
}
pub fn div(left:&Type,right:&Type) -> bool {
    left==right || (is_num(left) && is_num(right))
}
pub fn mdl(left:&Type,right:&Type) -> bool {
    left==right || (is_num(left) && is_num(right))
}

pub fn operation(op:&BinaryOp,left:&Type,right:&Type) -> bool {
    match op {
        BinaryOp::Add => add(left, right),
        BinaryOp::Sub => sub(left, right),
        BinaryOp::Mul => mul(left, right),
        BinaryOp::Div => div(left, right),
        BinaryOp::Mod => mdl(left, right),
        BinaryOp::And => bit(left, right),
        BinaryOp::Or => bit(left, right),
        _ => comp(left, right)
    }
}

fn type_from(l:&Type,r:&Type) -> Type {
    let t = match l {
        Type::Float => Type::Float,
        Type::Str => Type::Str,
        Type::Arr(t) => Type::Arr(t.clone()),
        Type::Int => match r {
            Type::Float => Type::Float,
            Type::Int => Type::Int,
            _ => Type::Unknown,
        }
        _ => l.clone()
    };
    Type::Option(vec![t])
}

pub fn get_type_by_op(op:&BinaryOp,l:&Type,r:&Type) -> Result<Type,CheckerError> {
    println!("{:?} {:?} {:?}",l,op,r);
    let t;
    if l == &Type::Unknown {
        if l == &Type::Unknown {
            t = get_undf(op)
        } else {
            t = get_undf_left_type(op, r)
        }
    } else if r == &Type::Unknown {
        t = get_undf_right_type(op, l)
    } else {
        if operation(op, l, r) {
            t = type_from(l, r)
        } else {
            return  Err(CheckerError::OperationError { op: op.clone(), left:l.clone(), right: r.clone() });
        }
    }
    match t  {
        Type::Option(t) => if t.len()==1 {
            return Ok(t.first().unwrap().clone())
        } else {
            return Ok(Type::Option(t.clone()))
        }
        _ => return Err(CheckerError::SomethingDoesntLineup { msg: "Got not Option type from get_type tree (std)".to_string() })?
    }
    
}
fn get_undf(op:&BinaryOp) -> Type {
    let mut opttps = Vec::new();
    for l in &Type::iter() {
        for r in &Type::iter() {
            if operation(op, l, r) {
                opttps.push(l.clone());
            }
        }
    }
    Type::Option(opttps)
}

fn get_undf_right_type(op:&BinaryOp,l:&Type) -> Type {
    let mut opttps = Vec::new();
    for t in Type::iter() {
        if operation(op, l, &t) {
            opttps.push(t);
        }
    }
    Type::Option(opttps)

}
fn get_undf_left_type(op:&BinaryOp,r:&Type) -> Type {
    let mut opttps = Vec::new();
    for t in Type::iter() {
        if operation(op, &t, r) {
            opttps.push(t);
        }
    }
    Type::Option(opttps)
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
