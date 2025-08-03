use std::{collections::HashMap, ops::Index};
use crate::{
    parser::{
        self, BinaryOp, EnumDef, EnumVariant, Expr, FieldDef, FunctionDef, Item, MatchArm, Parameter, Program, Stmt, UnaryOp, ValueDecl, Visibility
    },
    standart_lib::{comp_op, get_type_by_op, global_functions},
};

#[derive(Debug, Clone, PartialEq)]
pub enum CheckerError {
    KeyError {
        pos: String,
        ident: String,
        item: String,
    },
    UnaccesibleItemError,
    EntryFieldError,
    EntryComponentError,
    EntryFunctionError,
    OperationError {
        op: BinaryOp,
        left: Type,
        right: Type,
    },
    MatchEscapeError,
    ValueError {
        pos: String,
        exp: Type,
        found: Type,
    },
    NeedTypeAnnotationError {var:Value},
    SomethingDoesntLineup {
        msg: String,
    },
}

impl CheckerError {
    pub fn throw(self) {
        match self {
            Self::KeyError { pos, ident, item } => println!("KeyError: Not found {} {} in {}",item,ident,pos),
            Self::ValueError { pos, exp, found } => println!("ValueError: Expected {:?}, found {:?} in {}",exp,found,pos),
            _ => println!("{:?}",self)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PointerItem {
    pub index: usize,
    pub item_type: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Str,
    Bool,
    Arr(Box<Type>),
    Option(Vec<Type>),
    Custom(String),
    Unknown,
    Null,
}
impl Type {
    pub fn iter() -> Vec<Self> {
        let mut v = Type::bs_vec();
        v.extend(Type::arrt_vec());
        v
    }
    fn bs_vec() -> Vec<Self> {
        vec![Type::Int, Type::Float, Type::Str, Type::Bool, Type::Null]
    }
    fn arrt_vec() -> Vec<Self> {
        let mut arrv = Vec::new();
        for t in Type::bs_vec() {
            arrv.push(Type::Arr(Box::new(t)));
        }
        arrv.push(Type::Arr(Box::new(Type::Arr(Box::new(Type::Unknown)))));
        arrv
    }
    pub fn is(&self,t:&Type) -> bool {
        match self {
            Self::Option(tf) => match t {
                Self::Option(ts) => {for ft in ts {
                    if !tf.contains(ft) {return false}
                    } 
                    return true
                },
                _ => return tf.contains(t)
            }
            _ => self==t
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Num(f64),
    Str(String),
    Bool(bool),
    Arr(Vec<Value>),
    Custom(Field),
    Expr(Box<Expression>),
    Var(String),
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub components: HashMap<String, Comp>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Comp {
    pub requirecomp: Vec<String>,
    pub values: HashMap<String, Val>,
    pub impls: HashMap<String, Func>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Val {
    pub vis: Visibility,
    pub var: Var,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub variants: HashMap<String, Variant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    pub variant_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub vis: Visibility,
    pub ret: Type,
    pub args: HashMap<String, Var>,
    pub vars: HashMap<String, Var>,
    pub body: Vec<Command>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub var_type: Type,
    pub value: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Command {
    Declare {
        name: String,
        var_type: Type,
        value: Value,
    },
    Assign {
        target: String,
        value: Value,
    },
    MemberAssign {
        object: Value,
        member: String,
        value: Value,
    },
    IndexAssign {
        object: Value,
        index: Value,
        value: Value,
    },
    VoidCall {
        callee: String,
        args: Vec<Value>,
    },
    VoidMemberCall {
        object: Value,
        callee: Value,
        args: Vec<Value>,
    },
    Return(Value),
    Expr(Value),
    Match {
        object: Value,
        ret: Type,
        arms: Vec<Arm>,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arm {
    pattern: Value,
    body: Vec<Command>
}

pub enum checkReturn {
    None,
    Func(Func),    
    Field(Field),
    Comp(Comp),
    Body {
        vars: HashMap<String,Var>,
        args: HashMap<String,Var>,
        ret: Type,
        body: Vec<Command>
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Operation {
        left: Value,
        op: BinaryOp,
        right: Value,
    },
    Unary {
        op: UnaryOp,
        operand: Value,
    },
    Call {
        callee: String,
        args: Vec<Value>,
    },
    Member {
        object: Value,
        member: Value,
    },
    MemberCall {
        object: Value,
        callee: Value,
        args: Vec<Value>,
    },
    Index {
        object: Value,
        index: Value,
    },
}

pub fn operations(op: &BinaryOp, left: &Type, right: &Type) -> Result<Type, CheckerError> {
    get_type_by_op(op, left, right)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Checker {
    pub program: Program,
    pub current: usize,
    pub check: String,
    pub calls: HashMap<String, Vec<(String, Vec<Value>)>>,
    pub pointeritems: HashMap<String, PointerItem>,
    pub fields: HashMap<String, Field>,
    pub functions: HashMap<String, Func>,
    pub enums: HashMap<String, Enum>,
}

impl Checker {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            current: 0,
            check: "Program".to_string(),
            calls: HashMap::new(),
            pointeritems: HashMap::new(),
            fields: HashMap::new(),
            functions: global_functions(),
            enums: HashMap::new(),
        }
    }

    fn print_function(&self, a: &String, f: &Func, indent: &str) {
        println!("{}{:?} {:?} {}:", indent, f.vis, f.ret, a);
        print!("{}args: ( ", indent);
        for (n, v) in &f.args {
            print!("{}:{:?} = {:?}, ", n, v.var_type, v.value)
        }
        print!(")\n");
        println!("{}body {{", indent);
        for c in &f.body {
            print!("    {}", indent);
            self.print_command(c,format!("    {}", indent));
            print!(";\n")
        }
        println!("{}}}", indent);
    }

    fn print_field(&self, a: &String, f: &Field) {
        println!("{}:", a);
        for (a, c) in &f.components {
            self.print_component(a, c);
        }
    }

    fn field_string(&self, fl: &Field) -> String {
        for (a, f) in &self.fields {
            if f == fl {
                return a.to_string();
            }
        }
        return "Unknown".to_string();
    }

    fn print_component(&self, a: &String, c: &Comp) {
        print!("    req (");
        for r in &c.requirecomp {
            print!("{}, ", r);
        }
        print!(")\n");
        println!("    use {}:", a);
        println!("        val (");
        for (a, v) in &c.values {
            println!("            {:?} {} : {:?}", v.vis, a, v.var.var_type)
        }
        println!("        )");
        println!("        impl (");
        for (a, f) in &c.impls {
            self.print_function(a, f, "            ");
        }
        println!("        )")
    }

    fn print_command(&self, c: &Command,ident:String) {
        match c {
            Command::Assign { target, value } => {
                print!("{}=", target);
                self.print_value(value);
            }
            Command::Declare {
                name,
                var_type,
                value,
            } => {
                print!("{}:{:?}:=", name, var_type);
                self.print_value(value);
            }
            Command::IndexAssign {
                object,
                index,
                value,
            } => {
                self.print_value(object);
                print!("[");
                self.print_value(index);
                print!("]=");
                self.print_value(value);
            }
            Command::MemberAssign {
                object,
                member,
                value,
            } => {
                self.print_value(object);
                print!(".{}=", member);
                self.print_value(value);
            }
            Command::VoidCall { callee, args } => {
                print!("{}(", callee);
                for a in args {
                    self.print_value(a);
                    print!(",")
                }
                print!(")")
            }
            Command::VoidMemberCall {
                object,
                callee,
                args,
            } => {
                self.print_value(object);
                print!(".");
                self.print_value(callee);
                print!("(");
                for a in args {
                    self.print_value(a);
                    print!(",")
                }
                print!(")")
            }
            Command::Expr(v) => self.print_value(v),
            Command::Return(v) => {
                print!("return ");
                self.print_value(v);
            }
            Command::Match { object, arms, ret } => {
                print!("{:?} match ",ret);
                self.print_value(object);
                print!(":\n");
                for a in arms {
                    print!("    {}",ident);self.print_value(&a.pattern);print!(":\n");
                    for c in a.body.iter() {
                        print!("        {}",ident);self.print_command(c,"        ".to_string());print!(";\n");
                    }
                }
            }
            _ => (),
        }
    }

    fn print_value(&self, v: &Value) {
        match v {
            Value::Num(n) => print!("{}", n),
            Value::Str(s) => print!("\"{}\"", s),
            Value::Bool(b) => print!("{}", b),
            Value::Arr(a) => {
                for v in a {
                    self.print_value(v);
                    print!(",")
                }
            }
            Value::Custom(f) => print!("{:?}", self.field_string(f)),
            Value::Expr(e) => match (**e).clone() {
                Expression::Operation { left, op, right } => {
                    print!("(");
                    self.print_value(&left);
                    self.print_op(Some(op), None);
                    self.print_value(&right);
                    print!(")")
                }
                Expression::Unary { op, operand } => {
                    print!("(");
                    self.print_op(None, Some(op));
                    self.print_value(&operand);
                    print!(")")
                }
                Expression::Member { object, member } => {
                    self.print_value(&object);
                    print!(".");
                    self.print_value(&member);
                }
                Expression::Index { object, index } => {
                    self.print_value(&object);
                    print!("[");
                    self.print_value(&index);
                    print!("]");
                }
                Expression::Call { callee, args } => {
                    print!("{}", callee);
                    print!("(");
                    for a in args {
                        self.print_value(&a);
                        print!(",");
                    }
                    print!(")");
                }
                Expression::MemberCall {
                    object,
                    callee,
                    args,
                } => {
                    self.print_value(&object);
                    print!(".");
                    self.print_value(&callee);
                    print!("(");
                    for a in args {
                        self.print_value(&a);
                        print!(",");
                    }
                    print!(")");
                }
                _ => (),
            },
            Value::Var(v) => print!("{}", v),
            Value::Empty => print!("{:?}", v),
        }
    }

    fn print_op(&self, bop: Option<BinaryOp>, uop: Option<UnaryOp>) {
        if let Some(op) = bop {
            match op {
                BinaryOp::Add => print!("+"),
                BinaryOp::Sub => print!("-"),
                BinaryOp::Mul => print!("*"),
                BinaryOp::Div => print!("/"),
                BinaryOp::Mod => print!("%"),
                BinaryOp::Eq => print!("=="),
                BinaryOp::Gt => print!(">"),
                BinaryOp::Lt => print!("<"),
                BinaryOp::Gte => print!(">="),
                BinaryOp::Lte => print!("<="),
                BinaryOp::Ne => print!("!="),
                BinaryOp::And => print!("&"),
                BinaryOp::Or => print!("|"),
            }
        }
        if let Some(op) = uop {
            match op {
                UnaryOp::Not => print!("!"),
                UnaryOp::Neg => print!("-"),
            }
        }
    }

    fn print_call(&self, a: &String, c: &Vec<Value>) {
        print!("        {}(", a);
        for arg in c {
            self.print_value(&arg);
            print!(",")
        }
        print!(")\n")
    }

    pub fn check(&mut self) -> Result<checkReturn, CheckerError> {
        self.pointeritems = self.get_items()?;
        self.parse_top()?;
        self.check_main()?;
        self.check_top()?;
        println!("\nGlobal Functions:");
        for (a, f) in &self.functions {
            self.print_function(a, f, "");
        }
        println!("\nFields:");
        for (a, f) in &self.fields {
            self.print_field(a, f);
        }
        println!("Function calls:");
        for (a, v) in &self.calls {
            println!("    {} calls:", a);
            for (a, c) in v {
                self.print_call(a, c);
            }
        }
        //println!("{:?}",self.enums);
        Ok(checkReturn::None)
    }
    pub fn raw(&mut self) -> Result<checkReturn,CheckerError> {
        self.pointeritems = self.get_items()?;
        self.parse_top()?;
        self.check_main()?;
        self.check_top()?;
        for f in &self.fields {
            println!("{:?}",f)
        }
        Ok(checkReturn::None)
    }
    
    fn check_top(&mut self) -> Result<checkReturn, CheckerError> {
        for (a, f) in self.functions.clone() {
            let cf = match self.check_func(&f, "@Program@".to_string(),&a)? {
                checkReturn::Func(f) => f,
                _ => Err(CheckerError::SomethingDoesntLineup {
                    msg: "expected Func from self.check_func()".to_string(),
                })?,
            };
            self.functions.insert(a, cf);
        }
        for (a, f) in self.fields.clone() {
            let cf = match self.check_field(&f, a.clone())? {
                checkReturn::Field(f) => f,
                _ => Err(CheckerError::SomethingDoesntLineup {
                    msg: "expected Field from self.check_field()".to_string(),
                })?,
            };
            self.fields.insert(a, cf);
        }
        Ok(checkReturn::None)
    }

    pub fn get_type(
        &self,
        v: &Value,
        localvars: &HashMap<String, Var>,
        methods: &HashMap<String, Func>,
        p: &String
    ) -> Result<Type, CheckerError> {
        match v {
            Value::Num(n) => match n.fract() {
                0.0 => Ok(Type::Int),
                _ => Ok(Type::Float),
            },
            Value::Str(_) => Ok(Type::Str),
            Value::Bool(_) => Ok(Type::Bool),
            Value::Arr(a) => Ok(Type::Arr(Box::new(match a.get(0) {
                Some(v) => self.get_type(v, localvars, methods,p)?,
                None => Type::Unknown,
            }))),
            Value::Var(v) => match localvars.get(v) {
                Some(var) => Ok(var.var_type.clone()),
                None => match methods.get(v) {
                    Some(f) => Ok(f.ret.clone()),
                    None => match self.fields.get(v) {
                        Some(_) => Ok(Type::Custom(v.to_string())),
                        None => Err(CheckerError::KeyError {
                            pos: p.clone(),
                            ident: v.clone(),
                            item: "identifier".to_string(),
                        }),
                    }
                },
            },
            Value::Expr(e) => match (**e).clone() {
                Expression::Unary { op, operand } => match op {
                    UnaryOp::Not => Ok(Type::Bool),
                    UnaryOp::Neg => self.get_type(&operand, localvars, methods,p),
                },
                Expression::Operation { left, op, right } => operations(
                    &op,
                    &self.get_type(&left, localvars, methods,p)?,
                    &self.get_type(&right, localvars, methods,p)?,
                ),
                Expression::Call { callee, args } => match self.functions.get(&callee) {
                    Some(f) => {
                        let argtypes:Result<Vec<Type>,CheckerError> = args.iter().map(
                                |v| self.get_type(v, &localvars.clone(), &methods.clone(),p)
                            ).collect();
                        self.check_functions_args(
                            f,
                            argtypes?,
                            p
                        )?;
                        Ok(f.ret.clone())
                    },
                    None => Err(CheckerError::KeyError {
                        pos: p.clone(),
                        ident: callee,
                        item: "global function".to_string(),
                    }),
                },
                Expression::Index { object, index: _ } => {
                    let obj_type = self.get_type(&object, localvars, methods,p)?;
                    match obj_type {
                        Type::Arr(element_type) => Ok(*element_type),
                        _ => Err(CheckerError::ValueError {
                            pos: p.clone(),
                            exp: Type::Arr(Box::new(Type::Unknown)),
                            found: obj_type,
                        }),
                    }
                }
                Expression::Member { object, member } => {
                    let mut fieldvars = match object.clone() {
                        Value::Var(i) => match localvars.get(&i) {
                            Some(v) => match v.var_type.clone() {
                                Type::Custom(fs) => match self.fields.get(&fs) {
                                    Some(f) => self.get_field_values(&f),
                                    None => Err(CheckerError::SomethingDoesntLineup { msg: "No field".to_string() })?
                                }
                                _ => Err(CheckerError::NeedTypeAnnotationError { var: object } )?
                            }
                            None => Err(CheckerError::KeyError { pos: p.clone(),ident: i, item: "Object".to_string() })?
                        }
                        _ => Err(CheckerError::SomethingDoesntLineup { msg: "Gotten not a var from object".to_string() })?
                    };
                    fieldvars.extend(localvars.clone());
                    self.get_type(&member, &fieldvars, methods,p)
                }
                Expression::MemberCall {
                    object,
                    callee,
                    args: _,
                } => {
                    let mut fieldmethods = match object {
                        Value::Var(i) => match localvars.get(&i) {
                            Some(v) => match v.var_type.clone() {
                                Type::Custom(fs) => match self.fields.get(&fs) {
                                    Some(f) => self.get_field_methods(&f),
                                    None => Err(CheckerError::SomethingDoesntLineup { msg: "No field".to_string() })?
                                }
                                _ => Err(CheckerError::SomethingDoesntLineup { msg: "Not an object".to_string() })?
                            }
                            None => Err(CheckerError::KeyError { pos: p.clone(),ident: i, item: "object".to_string() })?
                        }
                        _ => Err(CheckerError::SomethingDoesntLineup { msg: "Gotten not a var from object".to_string() })?
                    };
                    fieldmethods.extend(methods.clone());
                    self.get_type(&callee, localvars, &fieldmethods,p)
                },
            },
            Value::Custom(f) => Ok(Type::Custom(self.field_string(f))),
            Value::Empty => return Err(CheckerError::NeedTypeAnnotationError { var: v.clone() })
        }
    }

    fn check_main(&self) -> Result<checkReturn, CheckerError> {
        let main_field = match self.fields.get("Main") {
            Some(m) => m.clone(),
            None => return Err(CheckerError::EntryFieldError),
        };

        let main_comp = match main_field.components.get("main") {
            Some(c) => c,
            None => return Err(CheckerError::EntryComponentError),
        };

        for (a, f) in &main_comp.impls {
            if [
                "new".to_string(),
                "main".to_string(),
                "start".to_string(),
                "run".to_string(),
            ]
            .contains(a)
                && f.vis == Visibility::Public
            {
                return Ok(checkReturn::None);
            }
        }
        Err(CheckerError::EntryFunctionError)
    }

    fn check_field(&mut self, f: &Field, fa: String) -> Result<checkReturn, CheckerError> {
        let mut comps = HashMap::new();
        for (a, c) in f.components.clone() {
            let cc = match self.check_comp(&c, fa.clone())? {
                checkReturn::Comp(c) => c,
                _ => Err(CheckerError::SomethingDoesntLineup {
                    msg: "expected Comp from self.check_comp()".to_string(),
                })?,
            };
            comps.insert(a, cc);
        }
        Ok(checkReturn::Field(Field { components: comps }))
    }

    fn check_comp(&mut self, c: &Comp, pf: String) -> Result<checkReturn, CheckerError> {
        let mut values = HashMap::new();
        let mut methods = HashMap::new();
        for (a, v) in c.values.clone() {
            values.insert(a, v);
        }
        for (a, f) in c.impls.clone() {
            let cf = match self.check_func(&f, pf.clone(),&a)? {
                checkReturn::Func(f) => f,
                _ => Err(CheckerError::SomethingDoesntLineup {
                    msg: "expected Func from self.check_func()".to_string(),
                })?,
            };
            methods.insert(a, cf);
        }

        Ok(checkReturn::Comp(Comp {
            requirecomp: c.requirecomp.clone(),
            values: values,
            impls: methods,
        }))
    }

    fn check_func(&mut self, f: &Func, pfield: String,fname:&String) -> Result<checkReturn, CheckerError> {
        match self.check_body(pfield, &f.vars, &f.args, &f.ret, &f.body, fname)? {
            checkReturn::Body { vars, args, ret, body } => Ok(checkReturn::Func(Func {
                vis: f.vis.clone(),
                ret,
                args,
                vars,
                body,
            })),
            _ => Ok(checkReturn::None)
        }
    }

    fn check_body(&mut self,pfield: String,vars:&HashMap<String,Var>,args:&HashMap<String,Var>,ret:&Type,body:&Vec<Command>,fname:&String) ->
        Result<checkReturn,CheckerError> {
        let mut fvars = args.clone();
        let mut fargs = args.clone();
        let mut fret = ret.clone();
        let mut optfret = Type::Null;
        let mut fbody = Vec::new();
        let pos = if pfield == "@Program@".to_string() {
            format!("global function {}",fname)
        } else {
            format!("{} method {}",pfield,fname)
        };
        for c in body.iter() {
            match c.clone() {
                Command::Declare {
                    name,
                    var_type,
                    value,
                } => {
                    self.record_call(&value, &pfield,&fvars);
                    let cvt = self.get_type(&value, &fvars, &HashMap::new(),&pos)?;
                    if cvt != var_type && var_type != Type::Unknown {
                        Err(CheckerError::ValueError { pos: format!("{} variable decalration {}",pos,name),exp: var_type, found: cvt.clone() })?
                    }
                    fvars.insert(
                        name.clone(),
                        Var {
                            var_type: cvt.clone(),
                            value: value.clone(),
                        },
                    );
                    fbody.push(Command::Declare {
                        name: name.clone(),
                        var_type: cvt.clone(),
                        value: value.clone(),
                    })
                }
                Command::Assign { target, value } => {
                    self.record_call(&value, &pfield,&fvars);
                    let v = fvars.get_mut(&target);
                    let v = match v {
                        Some(v) => v,
                        None => {
                            return Err(CheckerError::KeyError {
                                pos,
                                ident: target,
                                item: "variable".to_string(),
                            });
                        }
                    };
                    v.value = value.clone();
                    fbody.push(Command::Assign {
                        target,
                        value: value.clone(),
                    });
                }
                Command::Return(v) => {
                    self.record_call(&v, &pfield,&fvars);
                    let rettype = self.get_type(&v, &fvars, &HashMap::new(),&pos)?;
                    if fret == Type::Unknown {
                        fret = rettype.clone()
                    }
                    if fret != rettype {
                        Err(CheckerError::ValueError {
                            pos: format!("{} return",pos),
                            exp: fret.clone(),
                            found: rettype,
                        })?
                    }
                    fbody.push(Command::Return(v));
                }
                Command::Match { object, ret, arms } => {
                    self.record_call(&object, &pfield, &fvars);
                    let ot = self.get_type(&object, &fvars, &HashMap::new(),&pos)?;
                    fvars.insert("_".to_string(), Var {var_type:ot.clone(),value:Value::Empty});
                    let mut tps = Vec::new();
                    let mut narms = Vec::new();
                    let mut patterns = Vec::new();
                    for a in arms {
                        let mb = self.check_body(pfield.clone(), vars, args, &ret, &a.body,fname)?;
                        let b = match mb {
                            checkReturn::Body { vars, args, ret, body } => (vars,args,ret,body),
                            _ => (HashMap::new(),HashMap::new(),Type::Unknown,Vec::new())
                        };
                        let at = self.get_type(&a.pattern, &fvars, &HashMap::new(),&pos)?;
                        if at != ot {
                            Err(CheckerError::ValueError { pos:format!("{} match branch pattern",pos),exp: ot.clone(), found: at })?
                        }
                        patterns.push(a.pattern.clone());
                        for c in b.3.iter() {
                            let t = match c {
                                Command::Return(e) => self.get_type(e, &vars, &HashMap::new(),&pos)?,
                                _ => Type::Null
                            };
                            if !tps.contains(&t) {
                                tps.push(t);
                            }
                        }
                        narms.push(Arm {
                            pattern: a.pattern,
                            body: b.3
                            
                        });
                    }
                    if !patterns.contains(&Value::Var("_".to_string())) {Err(CheckerError::MatchEscapeError)?}
                    let t = if tps.iter().all(|x| x==tps.first().unwrap()) {
                        tps.first().unwrap().clone()
                    } else {
                        if tps.contains(&Type::Null) {
                            tps.retain(|t| t != &Type::Null);
                        }
                        Type::Option(tps)
                    };
                    optfret = t.clone();
                    fbody.push(Command::Match { object, ret: t, arms: narms });
                }
                
                Command::Expr(e) => match e {
                    Value::Expr(e) => match (*e).clone() {
                        Expression::Call { callee, args } => {
                            let mut arg_types = Vec::new();
                            for ca in &args {
                                let ct = self.get_type(ca, &fvars, &HashMap::new(),&pos)?;
                                arg_types.push(ct);
                            }

                            let func = match self.functions.get(&callee) {
                                Some(f) => f,
                                None => {
                                    return Err(CheckerError::KeyError {
                                        pos,
                                        ident: callee.clone(),
                                        item: "global function".to_string(),
                                    });
                                }
                            };
                            for ((ename, exp), ct) in func.args.iter().zip(arg_types) {
                                if exp.var_type != ct && exp.var_type != Type::Unknown {
                                    return Err(CheckerError::ValueError {
                                        pos: format!("{} argument {}",pos,ename),
                                        exp: exp.var_type.clone(),
                                        found: ct,
                                    });
                                }
                            }
                            match self.calls.get_mut("@Program@") {
                                Some(calls) => {
                                    calls.push((callee.clone(), args.clone()));
                                }
                                None => {
                                    self.calls.insert(
                                        "@Program@".to_string(),
                                        vec![(callee.clone(), args.clone())],
                                    );
                                }
                            }
                            fbody.push(Command::VoidCall {
                                callee: callee.clone(),
                                args,
                            });
                        }
                        Expression::MemberCall {
                            object,
                            callee,
                            args,
                        } => {
                            let cs = if let Value::Var(i) = callee.clone() {
                                i
                            } else {
                                return Err(CheckerError::SomethingDoesntLineup {
                                    msg: "Not found callee".to_string(),
                                });
                            };
                            let objs = if let Value::Var(i) = object.clone() {
                                i
                            } else {
                                return Err(CheckerError::SomethingDoesntLineup {
                                    msg: "Not found object".to_string(),
                                });
                            };
                            let refr = match fvars.get(&objs) {
                                Some(v) => match &v.var_type {
                                    Type::Custom(i) => i.clone(),
                                    _ => "@Primary@".to_string()
                                },
                                None => Err(CheckerError::KeyError {
                                    pos: pos.clone(),
                                    ident: objs.clone(),
                                    item: "variable".to_string(),
                                })?,
                            };
                            let o = match self.fields.get(&refr) {
                                Some(f) => f,
                                None => Err(CheckerError::KeyError {
                                    pos: pos.clone(),
                                    ident: objs.clone(),
                                    item: "field".to_string(),
                                })?,
                            };

                            let mut arg_types = Vec::new();
                            for ca in &args {
                                let ct = self.get_type(ca, &fvars, &HashMap::new(),&pos)?;
                                arg_types.push(ct);
                            }

                            let mthds = self.get_field_methods(o);
                            let func = match mthds.get(&cs) {
                                Some(f) => f,
                                None => {
                                    return Err(CheckerError::KeyError {
                                        pos,
                                        ident: cs.clone(),
                                        item: "Local function".to_string(),
                                    });
                                }
                            };

                            let mut args = Vec::new();
                            for ((ename, exp), ct) in func.args.iter().zip(arg_types) {
                                let mut arg = exp.clone();
                                if exp.var_type == Type::Unknown {
                                    arg.var_type = ct.clone();
                                }
                                if exp.var_type != ct {
                                    return Err(CheckerError::ValueError {
                                        pos:format!("{} argument {}",pos,ename),
                                        exp: exp.var_type.clone(),
                                        found: ct,
                                    });
                                }
                                args.push(exp.value.clone());
                            }
                            let c = match callee.clone() {
                                Value::Var(i) => i,
                                _ => Err(CheckerError::SomethingDoesntLineup {
                                    msg: "Expected var from callee in method call".to_string(),
                                })?,
                            };

                            match self.calls.get_mut(&refr) {
                                Some(calls) => {
                                    calls.push((c.clone(), args.clone()));
                                }
                                None => {
                                    self.calls
                                        .insert(refr.clone(), vec![(c.clone(), args.clone())]);
                                }
                            }
                            fbody.push(Command::VoidMemberCall {
                                object: object.clone(),
                                callee: callee.clone(),
                                args,
                            });
                        }
                        _ => (),
                    },
                    _ => (),
                },
                _ => (),
            }
        }
        fret = match fret {
            Type::Unknown => optfret,
            _ => fret,
        };
        
        Ok(checkReturn::Body { vars: fvars, args: fargs, ret: fret, body: fbody })
    }

    fn record_call(&mut self, v: &Value, f: &String,localvars: &HashMap<String, Var>) {
        match v {
            Value::Expr(e) => match (**e).clone() {
                Expression::Call { callee, args } => {
                    let pf = match self.functions.get(&callee) {
                        Some(_) => "@Program@".to_string(),
                        None => f.clone()
                    };
                    match self.calls.get_mut(&pf) {
                        Some(calls) => {
                            calls.push((callee, args));
                        }
                        None => {
                            self.calls.insert(pf, vec![(callee, args)]);
                        }
                    }
                },
                Expression::MemberCall {
                    object,
                    callee,
                    args,
                } => {
                    let o = match object {
                        Value::Var(i) => match localvars.get(&i) {
                            Some(v) => match v.var_type.clone() {
                                Type::Custom(fld) => fld.clone(),
                                _ => "@Undefined@".to_string(),
                            }
                            None => "@Undefined@".to_string(),
                        },
                        _ => "@Undefined@".to_string(),
                    };
                    let c = match callee {
                        Value::Var(i) => i,
                        _ => "@Undefined@".to_string(),
                    };
                    match self.calls.get_mut(&o) {
                        Some(calls) => {
                            calls.push((c, args));
                        }
                        None => {
                            self.calls.insert(o.clone(), vec![(c, args)]);
                        }
                    }
                }
                _ => ()
            },
            _ => (),
        }
    } 

    fn check_functions_args(&self,f:&Func,args:Vec<Type>,p:&String) -> Result<Type,CheckerError> {
        for ((ename,exp),found) in f.args.iter().zip(args) {
            if exp.var_type == Type::Unknown {}
            else if exp.var_type != found {
                return Err(CheckerError::ValueError { pos: format!("{} argument {}",p,ename),exp: exp.var_type.clone(), found: found.clone() })
            }
        }
        Ok(Type::Null)
    }

    fn get_field_methods(&self, f: &Field) -> HashMap<String, Func> {
        let mut methods = HashMap::new();
        for (_, c) in f.components.clone() {
            for (a, m) in c.impls {
                methods.insert(a, m);
            }
        }
        methods
    }
    fn get_field_values(&self, f: &Field) -> HashMap<String, Var> {
        let mut methods = HashMap::new();
        for (_, c) in f.components.clone() {
            for (a, v) in c.values {
                methods.insert(a, v.var.clone());
            }
        }
        methods
    }

    fn get_items(&mut self) -> Result<HashMap<String, PointerItem>, CheckerError> {
        let mut pointers = HashMap::new();
        for item in self.program.clone().items.iter() {
            let p = match item {
                Item::Function(def) => (def.name.clone(), self.pointer("Func")?),
                Item::Enum(def) => (def.name.clone(), self.pointer("Enum")?),
                Item::Field(def) => (def.name.clone(), self.pointer("Field")?),
                Item::Component(def) => (def.name.clone(), self.pointer("Comp")?),
                _ => return Err(CheckerError::UnaccesibleItemError),
            };
            pointers.insert(p.0, p.1);
            self.next();
        }
        Ok(pointers)
    }

    fn parse_top(&mut self) -> Result<bool, CheckerError> {
        for item in self.pointeritems.clone() {
            match item.1 {
                PointerItem { index, item_type } => {
                    if item_type == "Func" {
                        let d = self.program.items.get(index).unwrap();
                        match d {
                            Item::Function(def) => {
                                let f = self.parse_function(def.clone(), true, &"@".to_string())?;
                                self.functions.insert(def.name.clone(), f);
                            }
                            _ => (),
                        }
                    } else if item_type == "Field" {
                        let d = self.program.items.get(index).unwrap();
                        match d {
                            Item::Field(def) => {
                                let f = self.parse_field(def.name.clone())?;
                                self.fields.insert(def.name.clone(), f);
                            }
                            _ => (),
                        }
                    } else if item_type == "Enum" {
                        let d = self.program.items.get(index).unwrap();
                        match d {
                            Item::Enum(def) => {
                                let e = self.parse_enum(def.clone())?;
                                self.enums.insert(def.name.clone(), e);
                            }
                            _ => (),
                        }
                    }
                }
            }
        }
        Ok(true)
    }

    fn parse_field(&self, name: String) -> Result<Field, CheckerError> {
        let def = match self.pointeritems.get(&name) {
            Some(p) => p,
            None => {
                return Err(CheckerError::KeyError {
                    pos: "Program".to_string(),
                    ident: name.clone(),
                    item: "Field".to_string(),
                });
            }
        };
        let def = match self.program.clone().items.get(def.index) {
            Some(d) => match d {
                Item::Field(d) => d.clone(),
                _ => {
                    return Err(CheckerError::KeyError {
                        pos: "Program".to_string(),
                        ident: name,
                        item: "Field".to_string(),
                    });
                }
            },
            _ => {
                return Err(CheckerError::KeyError {
                    pos: "Program".to_string(),
                    ident: name,
                    item: "Field".to_string(),
                });
            }
        };
        Ok(Field {
            components: self.parse_components(def)?,
        })
    }

    fn parse_function(
        &self,
        f: FunctionDef,
        ispublic: bool,
        fl: &String,
    ) -> Result<Func, CheckerError> {
        Ok(Func {
            vis: match ispublic {
                false => f.visibility.clone(),
                true => Visibility::Public,
            },
            ret: self.parse_type(f.return_type)?,
            args: self.parse_parameters(f.parameters, fl)?,
            vars: HashMap::new(),
            body: self.parse_body(f.body.clone())?,
        })
    }

    fn parse_enum(&self, e: EnumDef) -> Result<Enum, CheckerError> {
        Ok(Enum {
            variants: self.parse_enum_variants(e.variants)?,
        })
    }

    fn parse_enum_variants(
        &self,
        vrnts: Vec<EnumVariant>,
    ) -> Result<HashMap<String, Variant>, CheckerError> {
        let mut variants = HashMap::new();
        for v in vrnts {
            variants.insert(
                v.name,
                Variant {
                    variant_type: self.parse_type(v.argtype)?,
                },
            );
        }
        Ok(variants)
    }

    fn parse_body(&self, b: Vec<Stmt>) -> Result<Vec<Command>, CheckerError> {
        let mut commands = Vec::new();
        for s in b {
            let c = match s {
                Stmt::VarDecl {
                    name: n,
                    type_annotation: t,
                    init: i,
                } => Some(Command::Declare {
                    name: n,
                    var_type: self.parse_type(t)?,
                    value: self.parse_expr(i)?,
                }),
                Stmt::Assignment { target, value } => Some(Command::Assign {
                    target,
                    value: self.parse_expr(value)?,
                }),
                Stmt::MemberAssignment {
                    object,
                    member,
                    value,
                } => Some(Command::MemberAssign {
                    object: self.parse_expr(object)?,
                    member,
                    value: self.parse_expr(value)?,
                }),
                Stmt::IndexAssignment {
                    object,
                    index,
                    value,
                } => Some(Command::IndexAssign {
                    object: self.parse_expr(object)?,
                    index: self.parse_expr(index)?,
                    value: self.parse_expr(value)?,
                }),
                Stmt::Expression(e) => Some(Command::Expr(self.parse_expr(e)?)),

                Stmt::Return(e) => Some(Command::Return(self.parse_expr(e)?)),

                Stmt::Match { expr, arms } => Some(Command::Match {
                    object: self.parse_expr(expr)?,
                    arms: self.parse_arms(arms)?,
                    ret: Type::Unknown
                }),
                
                _ => None,
            };
            if let Some(command) = c {
                commands.push(command);
            }
        }
        Ok(commands)
    }

    fn parse_arms(&self,arms:Vec<MatchArm>) -> Result<Vec<Arm>,CheckerError> {
        let mut parms = Vec::new();
        for a in arms {
            parms.push(Arm {
                pattern: self.parse_expr(a.pattern)?,
                body: self.parse_body(a.body)?,
            })
        }
        Ok(parms)
    }

    fn parse_expr(&self, e: Expr) -> Result<Value, CheckerError> {
        Ok(match e {
            Expr::Literal(l) => match l {
                parser::Literal::Number(n) => Value::Num(n),
                parser::Literal::String(s) => Value::Str(s),
                parser::Literal::Boolean(b) => Value::Bool(b),
                parser::Literal::None => Value::Empty,
            },
            Expr::Identifier(i) => Value::Var(i),
            Expr::ArrayLiteral(a) => {
                let v: Result<Vec<Value>, CheckerError> =
                    a.iter().map(|e| self.parse_expr(e.clone())).collect();
                Value::Arr(v?)
            }
            Expr::Binary { left, op, right } => Value::Expr(Box::new(Expression::Operation {
                left: self.parse_expr(*left.clone())?,
                op,
                right: self.parse_expr(*right.clone())?,
            })),
            Expr::Unary { op, operand } => Value::Expr(Box::new(Expression::Unary {
                op,
                operand: self.parse_expr(*operand.clone())?,
            })),
            Expr::Call { callee, args } => Value::Expr(Box::new(Expression::Call {
                callee,
                args: {
                    let a: Result<Vec<Value>, CheckerError> =
                        args.iter().map(|a| self.parse_expr(a.clone())).collect();
                    a?
                },
            })),
            Expr::MemberAccess { object, member } => Value::Expr(Box::new(Expression::Member {
                object: self.parse_expr(*object)?,
                member: Value::Var(member),
            })),
            Expr::MemberCall {
                object,
                method,
                args,
            } => Value::Expr(Box::new(Expression::MemberCall {
                object: self.parse_expr(*object)?,
                callee: Value::Var(method),
                args: {
                    let a: Result<Vec<Value>, CheckerError> =
                        args.iter().map(|a| self.parse_expr(a.clone())).collect();
                    a?
                },
            })),
            Expr::IndexAccess { object, index } => Value::Expr(Box::new(Expression::Index {
                object: self.parse_expr(*object)?,
                index: self.parse_expr(*index)?,
            })),
        })
    }

    fn parse_parameters(
        &self,
        prms: Vec<Parameter>,
        fl: &String,
    ) -> Result<HashMap<String, Var>, CheckerError> {
        let mut params = HashMap::new();
        for (i, p) in prms.iter().enumerate() {
            if fl != "@" && i == 0 {
                params.insert(
                    p.name.clone(),
                    Var {
                        var_type: Type::Custom(fl.to_string()),
                        value: Value::Empty,
                    },
                );
                continue;
            };
            params.insert(
                p.name.clone(),
                Var {
                    var_type: self.parse_type(p.type_annotation.clone())?,
                    value: Value::Empty,
                },
            );
        }
        Ok(params)
    }

    fn parse_components(&self, def: FieldDef) -> Result<HashMap<String, Comp>, CheckerError> {
        let mut components = HashMap::new();
        for compstr in def.components {
            components.insert(
                compstr.clone(),
                self.parse_component(&compstr, &def.name.clone())?,
            );
        }
        Ok(components)
    }

    fn parse_component(&self, s: &String, f: &String) -> Result<Comp, CheckerError> {
        let c = match self.pointeritems.get(s) {
            Some(c) => self.program.items.index(c.index),
            None => {
                return Err(CheckerError::KeyError {
                    pos: "Program".to_string(),
                    ident: s.clone(),
                    item: "component".to_string(),
                });
            }
        };
        let c = match c {
            Item::Component(d) => d,
            _ => {
                return Err(CheckerError::KeyError {
                    pos: "Program".to_string(),
                    ident: s.clone(),
                    item: "component".to_string(),
                });
            }
        };
        Ok(Comp {
            requirecomp: c.required_components.clone(),
            values: self.parse_values(c.values.clone())?,
            impls: self.parse_impls(c.functions.clone(), f)?,
        })
    }

    fn parse_impls(
        &self,
        impls: Vec<FunctionDef>,
        fl: &String,
    ) -> Result<HashMap<String, Func>, CheckerError> {
        let mut funcs = HashMap::new();
        for f in impls {
            funcs.insert(f.name.clone(), self.parse_function(f, false, fl)?);
        }
        Ok(funcs)
    }

    fn parse_values(&self, vls: Vec<ValueDecl>) -> Result<HashMap<String, Val>, CheckerError> {
        let mut values = HashMap::new();
        for v in vls {
            values.insert(
                v.name,
                Val {
                    vis: v.visibility,
                    var: self.parse_var(v.type_annotation)?,
                },
            );
        }
        Ok(values)
    }

    fn parse_var(&self, vt: Option<parser::Type>) -> Result<Var, CheckerError> {
        Ok(Var {
            var_type: self.parse_type(vt)?,
            value: Value::Empty,
        })
    }

    fn parse_type(&self, t: Option<parser::Type>) -> Result<Type, CheckerError> {
        Ok(match t {
            Some(parser::Type::Int) => Type::Int,
            Some(parser::Type::Float) => Type::Float,
            Some(parser::Type::Bool) => Type::Bool,
            Some(parser::Type::Str) => Type::Str,
            Some(parser::Type::None) => Type::Null,
            Some(parser::Type::Arr(t)) => Type::Arr(Box::new(self.parse_type(*t)?)),
            Some(parser::Type::Option(tps)) => {
                let mapped: Result<Vec<Type>, CheckerError> = tps
                    .iter()
                    .map(|t| self.parse_type(Some(t.clone())))
                    .collect();
                Type::Option(mapped?)
            }
            Some(parser::Type::Custom(f)) => Type::Custom(f),
            None => Type::Unknown,
        })
    }

    fn pointer(&self, t: &str) -> Result<PointerItem, CheckerError> {
        Ok(PointerItem {
            index: self.current,
            item_type: t.to_string(),
        })
    }

    fn next(&mut self) {
        self.current += 1
    }
}
