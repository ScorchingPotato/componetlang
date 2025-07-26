use std::{collections::HashMap, ops::Index};

use crate::{parser::{self, BinaryOp, EnumDef, EnumVariant, Expr, FieldDef, FunctionDef, Item, Parameter, Program, Stmt, UnaryOp, ValueDecl, Visibility}, standart_lib::{comp_op, global_functions}};
use crate::standart_lib::{bool_op,num_op,int_float_op,str_op,str_int_op};

#[derive(Debug,Clone,PartialEq)]
pub enum CheckerError {
    KeyError {ident:String,item:String},
    UnaccesibleItemError,
    EntryFieldError,
    EntryComponentError,
    EntryFunctionError,
    OperationError {op:BinaryOp,left:Type,right:Type},
    ValueError {exp:Type,found:Type},
    SomethingDoesntLineup {msg:String}
}

#[derive(Debug,Clone,PartialEq)]
pub struct PointerItem {
    pub index: usize,
    pub item_type: String
}

#[derive(Debug,Clone,PartialEq)]
pub enum Type {
    Int,
    Float,
    Str,
    Bool,
    Arr(Box<Type>),
    Option(Vec<Type>),
    Custom(String),
    Unknown,
    Null
}

#[derive(Debug,Clone,PartialEq)]
pub enum Value {
    Num(f64),
    Str(String),
    Bool(bool),
    Arr(Vec<Value>),
    Custom(Field),
    Expr(Box<Expression>),
    Var(String),
    Empty
}

#[derive(Debug,Clone,PartialEq)]
pub struct Field {
    pub components: HashMap<String,Comp>
}

#[derive(Debug,Clone,PartialEq)]
pub struct Comp {
    pub requirecomp: Vec<String>,
    pub values: HashMap<String,Val>,
    pub impls: HashMap<String,Func>
}

#[derive(Debug,Clone,PartialEq)]
pub struct Val {
    pub vis: Visibility,
    pub var: Var,
}

#[derive(Debug,Clone,PartialEq)]
pub struct Enum {
    pub variants: HashMap<String,Variant>
}

#[derive(Debug,Clone,PartialEq)]
pub struct Variant {
    pub variant_type: Type
}

#[derive(Debug,Clone,PartialEq)]
pub struct Func {
    pub vis: Visibility,
    pub ret: Type,
    pub args: HashMap<String,Var>,
    pub vars: HashMap<String,Var>,
    pub body: Vec<Command>
}

#[derive(Debug,Clone,PartialEq)]
pub struct Var {
    pub var_type: Type,
    pub value: Value,
}

#[derive(Debug,Clone,PartialEq)]
pub enum Command {
    Declare {
        name: String,
        var_type: Type,
        value: Value
    },
    Assign {
        target: String,
        value: Value
    },
    MemberAssign {
        object: Value,
        member: String,
        value: Value
    },
    IndexAssign {
        object: Value,
        index: Value,
        value: Value
    },
    Return(Value),
    Expr(Value)
}

pub enum checkReturn {
    None,
    Func(Func),
    Field(Field),
    Comp(Comp)
}

#[derive(Debug,Clone,PartialEq)]
pub enum Expression {
    Operation {left:Value,op:BinaryOp,right:Value},
    Unary {op:UnaryOp,operand:Value},
    Call {callee:String,args:Vec<Value>},
    Member {object:Value,member:Value},
    MemberCall {object:Value,callee:Value,args:Vec<Value>},
    Index {object:Value,index:Value}
}

pub fn operations(op:&BinaryOp,left:&Type,right:&Type) -> Result<Type,CheckerError> {
    if left == &Type::Bool && right == &Type::Bool {
        if bool_op().contains(op) {
            Ok(Type::Bool)
        } else {
            Err(CheckerError::OperationError { op: op.clone(), left: left.clone(), right: right.clone() })
        }
    } else if left == &Type::Int {
        if right == &Type::Int {
            if num_op().contains(op) {Ok(Type::Int)}
            else {Err(CheckerError::OperationError { op: op.clone(), left: left.clone(), right: right.clone() })}
        } else if right == &Type::Float {
            if int_float_op().contains(op) {Ok(Type::Float)}
            else {Err(CheckerError::OperationError { op: op.clone(), left: left.clone(), right: right.clone() })}
        } else {
            Err(CheckerError::OperationError { op: op.clone(), left: left.clone(), right: right.clone() })
        }
    } else if left == &Type::Float {
        if right == &Type::Float {
            if num_op().contains(op) {Ok(Type::Float)}
            else {Err(CheckerError::OperationError { op: op.clone(), left: left.clone(), right: right.clone() })}
        } else if right == &Type::Int {
            if int_float_op().contains(op) {Ok(Type::Float)}
            else {Err(CheckerError::OperationError { op: op.clone(), left: left.clone(), right: right.clone() })}
        } else {
            Err(CheckerError::OperationError { op: op.clone(), left: left.clone(), right: right.clone() })
        }
    } else if left == &Type::Str {
        if right == &Type::Str {
            if str_op().contains(op) {Ok(Type::Str)}
            else {Err(CheckerError::OperationError { op: op.clone(), left: left.clone(), right: right.clone() })}
        } else if right == &Type::Int {
            if str_int_op().contains(op) {Ok(Type::Str)}
            else {Err(CheckerError::OperationError { op: op.clone(), left: left.clone(), right: right.clone() })}
        } else {
            Err(CheckerError::OperationError { op: op.clone(), left: left.clone(), right: right.clone() })
        }
    } else if left != &Type::Unknown {
        if right == left {
            if comp_op().contains(op) {Ok(right.clone())}
            else {Err(CheckerError::OperationError { op: op.clone(), left: left.clone(), right: right.clone() })}
        } else {
            Err(CheckerError::OperationError { op: op.clone(), left: left.clone(), right: right.clone() })
        }
    } else {
        Ok(Type::Unknown)
        //Err(CheckerError::OperationError { op: op.clone(), left: left.clone(), right: right.clone() })
    }
}

#[derive(Debug,Clone,PartialEq)]
pub struct Checker {
    pub program: Program,
    pub current: usize,
    pub check: String,
    pub pointeritems: HashMap<String,PointerItem>,
    pub fields: HashMap<String,Field>,
    pub functions: HashMap<String,Func>,
    pub enums: HashMap<String,Enum>
}

impl Checker {
    pub fn new(program:Program) -> Self {
        Self {
            program,
            current: 0,
            check: "Program".to_string(),
            pointeritems: HashMap::new(),
            fields: HashMap::new(),
            functions: global_functions(),
            enums: HashMap::new(),
        }
    }

    fn print_function(&self,a:&String,f:&Func,indent:&str) {
        println!("{}{:?} {:?} {}:",indent,f.vis,f.ret,a);
        print!("{}args: ( ",indent);
        for (n,v) in &f.args {
            print!("{}:{:?} = {:?}, ",n,v.var_type,v.value)
        }
        print!(")\n");
        println!("{}body {{",indent);
        for c in &f.body {
            print!("    {}",indent);self.print_command(c);print!(";\n")
        }
        println!("{}}}",indent);
    }
    
    fn print_field(&self,a:&String,f:&Field) {
        println!("{}:",a);
        for (a,c) in &f.components {
            self.print_component(a,c);
        }
    }
    
    fn field_string(&self,fl:&Field) -> String {
        for (a,f) in &self.fields {
            if f == fl {
                return a.to_string()
            }
        };
        return "Unknown".to_string()
    }
    
    fn print_component(&self,a:&String,c:&Comp) {
        print!("    req (");
        for r in &c.requirecomp {
            print!("{}, ",r);
        }
        print!(")\n");
        println!("    use {}:",a);
        println!("        val (");
        for (a,v) in &c.values {
            println!("            {:?} {} : {:?}",v.vis,a,v.var.var_type)
        }
        println!("        )");
        println!("        impl (");
        for (a,f) in &c.impls {
            self.print_function(a, f, "            ");
        }
        println!("        )")
    }
    
    fn print_command(&self,c:&Command) {
        match c {
            Command::Assign { target, value } => {
                print!("{}=",target);
                self.print_value(value);
            }
            Command::Declare { name, var_type, value } => {
                print!("{}:{:?}:=",name,var_type);
                self.print_value(value);
            }
            Command::IndexAssign { object, index, value } => {
                self.print_value(object);
                print!("[");self.print_value(index);print!("]=");
                self.print_value(value);
            }
            Command::MemberAssign { object, member, value } => {
                self.print_value(object);
                print!(".{}=",member);
                self.print_value(value);
            }
            Command::Expr(v) => self.print_value(v),
            Command::Return(v) => {
                print!("return ");self.print_value(v);
            }
            _ => ()
        }
    }
    
    fn print_value(&self,v:&Value) {
        match v {
            Value::Num(n) => print!("{}",n),
            Value::Str(s) => print!("\"{}\"",s),
            Value::Bool(b) => print!("{}",b),
            Value::Arr(a) => for v in a {self.print_value(v);print!(",")},
            Value::Custom(f) => print!("{:?}",self.field_string(f)),
            Value::Expr(e) => match (**e).clone() {
                Expression::Operation { left, op, right } => {
                    print!("(");
                    self.print_value(&left);
                    self.print_op(Some(op),None);
                    self.print_value(&right);
                    print!(")")
                }
                Expression::Unary { op, operand } => {
                    print!("(");
                    self.print_op(None,Some(op));
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
                    print!("{}",callee);
                    print!("(");
                    for a in args {
                        self.print_value(&a);
                        print!(",");
                    }
                    print!(")");
                }
                Expression::MemberCall { object, callee, args } => {
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
            }
            Value::Var(v) => print!("{}",v),
            Value::Empty => print!("~")
        }
    }
    
    fn print_op(&self,bop:Option<BinaryOp>,uop:Option<UnaryOp>) {
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

    pub fn check(&mut self) -> Result<checkReturn,CheckerError> {
        self.pointeritems = self.get_items()?;
        self.parse_top()?;
        self.check_main()?;
        self.check_top()?;
        println!("\nGlobal Functions:");
        for (a,f) in &self.functions {
            self.print_function(a, f,"");
        }
        println!("\nFields:");
        for (a,f) in &self.fields {
            self.print_field(a, f);
        }
        println!("{:?}",self.enums);
        Ok(checkReturn::None)
    }
    fn check_top(&mut self) -> Result<checkReturn,CheckerError> {
        for (a,f) in self.functions.clone() {
            let cf = match self.check_func(&f,"@Program@".to_string())? {checkReturn::Func(f) => f,_ => Err(CheckerError::SomethingDoesntLineup { msg: "expected Func from self.check_func()".to_string() })?};
            self.functions.insert(a, cf);
        }
        for (a,f) in self.fields.clone() {
            let cf = match self.check_field(&f,a.clone())? {checkReturn::Field(f) => f,_ => Err(CheckerError::SomethingDoesntLineup { msg: "expected Field from self.check_field()".to_string() })?};
            self.fields.insert(a, cf);
        }
        Ok(checkReturn::None)
    }

    pub fn get_type(&self,v:&Value,localvars:&HashMap<String,Var>,methods:&HashMap<String,Func>) -> Result<Type,CheckerError> {
        match v {
            Value::Num(n) => match n.fract() {
                0.0 => Ok(Type::Int),
                _ => Ok(Type::Float)
            }
            Value::Str(_) => Ok(Type::Str),
            Value::Bool(_) => Ok(Type::Bool),
            Value::Arr(a) => Ok(Type::Arr(Box::new(match a.get(0) {
                Some(v) => self.get_type(v, localvars,methods)?,
                None => Type::Unknown 
            }))),
            Value::Var(v) => match localvars.get(v) {
                Some(var) => Ok(var.var_type.clone()),
                None => match methods.get(v) {
                    Some(f) => Ok(f.ret.clone()),
                    None => Err(CheckerError::KeyError { ident: v.clone(), item: "Variable or Function".to_string() })
                }
            },
            Value::Expr(e) => match (**e).clone() {
                Expression::Unary { op, operand } => match op {
                    UnaryOp::Not => Ok(Type::Bool),
                    UnaryOp::Neg => self.get_type(&operand, localvars,methods)
                }
                Expression::Operation { left, op, right } => operations(&op, &self.get_type(&left, localvars,methods)?, &self.get_type(&right, localvars,methods)?),
                Expression::Call { callee, args: _ } => match self.functions.get(&callee) {
                    Some(f) => Ok(f.ret.clone()),
                    None => Err(CheckerError::KeyError { ident: callee, item: "Global function".to_string() })
                }
                Expression::Index { object, index: _ } => {
                    let obj_type = self.get_type(&object, localvars,methods)?;
                    match obj_type {
                        Type::Arr(element_type) => Ok(*element_type),
                        _ => Err(CheckerError::ValueError { 
                            exp: Type::Arr(Box::new(Type::Unknown)), 
                            found: obj_type 
                        })
                    }
                }
                Expression::Member { object: _, member } => self.get_type(&member, localvars,methods),
                Expression::MemberCall { object: _, callee, args: _ } => self.get_type(&callee, localvars,methods)
            }
            Value::Custom(f) => Ok(Type::Custom(self.field_string(f))),
            Value::Empty => Ok(Type::Unknown)
        }
    }
    
    fn check_main(&self) -> Result<checkReturn,CheckerError> {
        let main_field = match self.fields.get("Main") {
            Some(m) => m.clone(),
            None => return Err(CheckerError::EntryFieldError)
        };

        let main_comp = match main_field.components.get("main") {
            Some(c) => c,
            None => return Err(CheckerError::EntryComponentError)
        };

        for (a,f) in &main_comp.impls {
            if ["new".to_string(),"main".to_string(),"start".to_string(),"run".to_string()].contains(a) && f.vis==Visibility::Public {
                return Ok(checkReturn::None)
            }
        };
        Err(CheckerError::EntryFunctionError)
    }

    fn check_field(&mut self,f:&Field,fa:String) -> Result<checkReturn,CheckerError> {
        let mut comps = HashMap::new();
        for (a,c) in f.components.clone() {
            let cc = match self.check_comp(&c,fa.clone())? {checkReturn::Comp(c) => c,_ => Err(CheckerError::SomethingDoesntLineup { msg: "expected Comp from self.check_comp()".to_string() })?};
            comps.insert(a, cc);
        }
        Ok(checkReturn::Field(Field {
            components: comps
        }))
    }

    fn check_comp(&mut self,c:&Comp,pf:String) -> Result<checkReturn,CheckerError> {
        let mut values = HashMap::new();
        let mut methods = HashMap::new();
        for (a,v) in c.values.clone() {
            values.insert(a, v);
        }
        for (a,f) in c.impls.clone() {
            let cf = match self.check_func(&f, pf.clone())? {checkReturn::Func(f) => f,_ => Err(CheckerError::SomethingDoesntLineup { msg: "expected Func from self.check_func()".to_string() })?};
            methods.insert(a, cf);
        }

        Ok(checkReturn::Comp(Comp {
            requirecomp: c.requirecomp.clone(),
            values: values,
            impls: methods
        }))
    }

    fn check_func(&mut self,f:&Func, pfield: String) -> Result<checkReturn,CheckerError> {
        let mut fvars = f.args.clone();
        let mut fargs = f.args.clone();
        let mut fret = f.ret.clone();
        let mut fbody = f.body.clone();
        let mut methods = self.functions.clone();
        if pfield != "@Program@".to_string() {
            let f = match self.fields.get(&pfield) {
                Some(f) => f,
                None => Err(CheckerError::KeyError { ident: pfield.clone(), item: "Field".to_string() })?
            };
            fvars.extend(self.get_field_values(f));
            methods.extend(self.get_field_methods(f));
        }
        for c in fbody.iter() {
            match c.clone() {
                Command::Declare { name, var_type, value } => {
                    let cvt = self.get_type(&value, &fvars,&methods)?;
                    fvars.insert(name, Var { var_type:cvt, value });
                },
                Command::Assign { target, value } => {
                    let v = fvars.get_mut(&target);
                    let v = match v {
                        Some(v) => v,
                        None => return Err(CheckerError::KeyError { ident: target, item: "Variable".to_string() })
                    };
                    v.value = value;
                },
                Command::Return(v) => {
                    let rettype = self.get_type(&v, &fvars,&methods)?;
                    if fret == Type::Unknown {
                        fret = rettype.clone()
                    }
                    if fret != rettype {
                        Err(CheckerError::ValueError { exp: fret.clone(), found: rettype })?
                    }
                }
                Command::Expr(e) => match e {
                    Value::Expr(e) => match (*e).clone() {
                        Expression::Call { callee, args } => {
                            // First collect the argument types
                            let mut arg_types = Vec::new();
                            for ca in &args {
                                let ct = self.get_type(ca, &fvars,&methods)?;
                                arg_types.push(ct);
                            }
                            
                            // Then modify the function arguments
                            let func = match self.functions.get_mut(&callee) {
                                Some(f) => f,
                                None => return Err(CheckerError::KeyError { ident: callee.clone(), item: "Global function".to_string() })
                            };
                            
                            for ((_ename,exp), ct) in func.args.iter_mut().zip(arg_types) {
                                if exp.var_type == Type::Unknown {
                                    exp.var_type = ct.clone();
                                }
                                if exp.var_type != ct {
                                    return Err(CheckerError::ValueError { exp: exp.var_type.clone(), found: ct });
                                }
                            }
                        }
                        Expression::MemberCall { object, callee, args } => {
                            let cs = if let Value::Var(i) = callee {i} else {return Err(CheckerError::SomethingDoesntLineup {msg:"Not found callee".to_string()})};
                            let o = match self.fields.get(&pfield) {Some(f) => f,None => Err(CheckerError::KeyError { ident: pfield.clone(), item: "Field".to_string() })?};
                            
                            // First collect the argument types
                            let mut arg_types = Vec::new();
                            for ca in &args {
                                let ct = self.get_type(ca, &f.vars,&methods)?;
                                arg_types.push(ct);
                            }
                            
                            // Then modify the method arguments
                            let mut mthds = self.get_field_methods(o);
                            let func = match mthds.get_mut(&cs) {
                                Some(f) => f,
                                None => return Err(CheckerError::KeyError { ident: cs.clone(), item: "Local function".to_string() })
                            };
                            
                            for ((_ename,exp), ct) in func.args.iter_mut().zip(arg_types) {
                                if exp.var_type == Type::Unknown {
                                    exp.var_type = ct.clone();
                                }
                                if exp.var_type != ct {
                                    return Err(CheckerError::ValueError { exp: exp.var_type.clone(), found: ct });
                                }
                            }
                        }
                        _ => ()
                    }
                    _ => ()
                }
                _ => ()
            }
        }

        fret = match fret {Type::Unknown => Type::Null,_ => fret};

        Ok(checkReturn::Func(Func {
            vis: f.vis.clone(),
            ret: fret,
            args: fargs,
            vars: fvars,
            body: fbody
        }))
    }

    fn get_field_methods(&self,f:&Field) -> HashMap<String, Func> {
        let mut methods =  HashMap::new();
        for (_,c) in f.components.clone() {
            for (a,m) in c.impls {
                methods.insert(a, m);
            }
        }
        methods
    }
    fn get_field_values(&self,f:&Field) -> HashMap<String,Var> {
        let mut methods =  HashMap::new();
        for (_,c) in f.components.clone() {
            for (a,v) in c.values {
                methods.insert(a, v.var.clone());
            }
        }
        methods
    }

    fn get_items(&mut self) -> Result<HashMap<String,PointerItem>,CheckerError> {
        let mut pointers = HashMap::new();
        for item in self.program.clone().items.iter() {
            let p = match item {
                Item::Function(def) => (def.name.clone(),self.pointer("Func")?),
                Item::Enum(def) => (def.name.clone(),self.pointer("Enum")?),
                Item::Field(def) => (def.name.clone(),self.pointer("Field")?),
                Item::Component(def) => (def.name.clone(),self.pointer("Comp")?),
                _ => return Err(CheckerError::UnaccesibleItemError)
            };
            pointers.insert(p.0,p.1);
            self.next();
        }
        Ok(pointers)
    }
    
    fn parse_top(&mut self) -> Result<bool,CheckerError>{
        for item in self.pointeritems.clone() {
            match item.1 {
                PointerItem { index, item_type } => {
                    if item_type == "Func" {
                        let d = self.program.items.get(index).unwrap(); 
                        match d {
                            Item::Function(def) => {
                                let f = self.parse_function(def.clone(),true,&"@".to_string())?;
                                self.functions.insert(def.name.clone(), f);
                            }
                            _ => ()
                        }
                    } else if item_type == "Field" {
                        let d = self.program.items.get(index).unwrap(); 
                        match d {
                            Item::Field(def) => {
                                let f = self.parse_field(def.name.clone())?;
                                self.fields.insert(def.name.clone(), f);
                            }
                            _ => ()
                        }
                    } else if item_type == "Enum" {
                        let d = self.program.items.get(index).unwrap(); 
                        match d {
                            Item::Enum(def) => {
                                let e = self.parse_enum(def.clone())?;
                                self.enums.insert(def.name.clone(), e);
                            }
                            _ => ()
                        }
                    }
                }
            }
        }
        Ok(true)
    }

    fn parse_field(&self,name:String) -> Result<Field,CheckerError> {
        let def = match self.pointeritems.get(&name) {
            Some(p) => p,
            None => return Err(CheckerError::KeyError {ident:name.clone(),item:"Field".to_string()})
        };
        let def = match self.program.clone().items.get(def.index) {
            Some(d) => match d {
                Item::Field(d) => d.clone(),
                _ => return Err(CheckerError::KeyError {ident:name,item:"Field".to_string()})
            }
            _ => return Err(CheckerError::KeyError {ident:name,item:"Field".to_string()})
        };
        Ok(Field {
            components: self.parse_components(def)?
        })
    }

    fn parse_function(&self,f:FunctionDef,ispublic:bool,fl:&String) -> Result<Func,CheckerError> {
        Ok(Func {
            vis: match ispublic { false => f.visibility.clone(), true=>Visibility::Public },
            ret: self.parse_type(f.return_type)?,
            args: self.parse_parameters(f.parameters,fl)?,
            vars: HashMap::new(),
            body: self.parse_body(f.body.clone())?
        })
    }

    fn parse_enum(&self,e:EnumDef) -> Result<Enum,CheckerError> {
        Ok(Enum {
            variants: self.parse_enum_variants(e.variants)?
        })
    }

    fn parse_enum_variants(&self,vrnts:Vec<EnumVariant>) -> Result<HashMap<String,Variant>,CheckerError> {
        let mut variants = HashMap::new();
        for v in vrnts {
            variants.insert(v.name, Variant{ variant_type: self.parse_type(v.argtype)?});
        }
        Ok(variants)
    }

    fn parse_body(&self,b:Vec<Stmt>) -> Result<Vec<Command>,CheckerError> {
        let mut commands = Vec::new();
        for s in b {
            let c = match s {
                Stmt::VarDecl {name:n,type_annotation:t,init:i} => Some(Command::Declare { name: n, var_type: self.parse_type(t)?, value: self.parse_expr(i)? }),
                Stmt::Assignment { target, value } => Some(Command::Assign { target, value: self.parse_expr(value)? }),
                Stmt::MemberAssignment { object, member, value } => Some(Command::MemberAssign { object: self.parse_expr(object)?, member, value: self.parse_expr(value)? }),
                Stmt::IndexAssignment { object, index, value } => Some(Command::IndexAssign { object: self.parse_expr(object)?, index: self.parse_expr(index)?, value: self.parse_expr(value)? }),
                Stmt::Expression(e) => Some(Command::Expr(self.parse_expr(e)?)),

                Stmt::Return(e) => Some(Command::Return(self.parse_expr(e)?)),

                _ => None
            };
            if let Some(command) = c {
                commands.push(command);
            }
        }
        Ok(commands)
    }

    fn parse_expr(&self, e:Expr) -> Result<Value,CheckerError> {
        Ok(match e {
            Expr::Literal(l) => match l {
                parser::Literal::Number(n) => Value::Num(n),
                parser::Literal::String(s) => Value::Str(s),
                parser::Literal::Boolean(b) => Value::Bool(b),
                parser::Literal::None => Value::Empty
            },
            Expr::Identifier(i) => Value::Var(i),
            Expr::ArrayLiteral(a) => {
                let v:Result<Vec<Value>,CheckerError> = a.iter().map(|e| self.parse_expr(e.clone())).collect();
                Value::Arr(v?)
            },
            Expr::Binary { left, op, right } => Value::Expr(Box::new(Expression::Operation { left:self.parse_expr(*left.clone())?, op, right:self.parse_expr(*right.clone())? })),
            Expr::Unary { op, operand } => Value::Expr(Box::new(Expression::Unary { op, operand:self.parse_expr(*operand.clone())?})),
            Expr::Call { callee, args } => Value::Expr(Box::new(Expression::Call { callee, args: {
                let a:Result<Vec<Value>,CheckerError> = args.iter().map(|a| self.parse_expr(a.clone())).collect();
                a?
            } })),
            Expr::MemberAccess { object, member } => Value::Expr(Box::new(Expression::Member { object: self.parse_expr(*object)?, member: Value::Var(member) })),
            Expr::MemberCall { object, method, args } => Value::Expr(Box::new(Expression::MemberCall { object: self.parse_expr(*object)?, callee: Value::Var(method),
                args: {
                    let a:Result<Vec<Value>,CheckerError> = args.iter().map(|a| self.parse_expr(a.clone())).collect();
                    a?
                } 
            })),
            Expr::IndexAccess { object, index } => Value::Expr(Box::new(Expression::Index { object:self.parse_expr(*object)?, index:self.parse_expr(*index)? }))
        })
    }

    fn parse_parameters(&self,prms:Vec<Parameter>,fl:&String) -> Result<HashMap<String,Var>,CheckerError> {
        let mut params = HashMap::new();
        for (i,p) in prms.iter().enumerate() {
            if fl != "@" && i==0 {
                println!("Parsing method");
                params.insert(p.name.clone(), Var {
                    var_type: Type::Custom(fl.to_string()),
                    value: Value::Empty
                });
                continue
            };
            params.insert(p.name.clone(), Var {
                var_type: self.parse_type(p.type_annotation.clone())?,
                value: Value::Empty
            });
        }
        Ok(params)
    }

    fn parse_components(&self,def:FieldDef) -> Result<HashMap<String,Comp>,CheckerError> {
        let mut components = HashMap::new();
        for compstr in def.components {
            components.insert(compstr.clone(),self.parse_component(&compstr,&def.name.clone())?);
        }
        Ok(components)
    }

    fn parse_component(&self,s:&String,f:&String) -> Result<Comp,CheckerError> {
        let c = match self.pointeritems.get(s) {
            Some(c) => self.program.items.index(c.index),
            None => return Err(CheckerError::KeyError {ident:s.clone(),item:"Comp".to_string()})
        };
        let c = match c {
            Item::Component(d) => d,
            _ => return Err(CheckerError::KeyError{ident:s.clone(),item:"Comp".to_string()})
        };
        Ok(Comp {
            requirecomp: c.required_components.clone(),
            values: self.parse_values(c.values.clone())?,
            impls: self.parse_impls(c.functions.clone(),f)?
        })
    }

    fn parse_impls(&self,impls:Vec<FunctionDef>,fl:&String) -> Result<HashMap<String,Func>,CheckerError> {
        let mut funcs = HashMap::new();
        for f in impls {
            funcs.insert(f.name.clone(),self.parse_function(f,false,fl)?);
        }
        Ok(funcs)
    }

    fn parse_values(&self,vls:Vec<ValueDecl>) -> Result<HashMap<String,Val>,CheckerError> {
        let mut values = HashMap::new();
        for v in vls {
            values.insert(v.name,Val {
                vis: v.visibility,
                var: self.parse_var(v.type_annotation)?
            });
        }
        Ok(values)
    }

    fn parse_var(&self,vt:Option<parser::Type>) -> Result<Var,CheckerError> {
        Ok(Var {
            var_type: self.parse_type(vt)?,
            value: Value::Empty
        })
    }

    fn parse_type(&self,t:Option<parser::Type>) -> Result<Type,CheckerError> {
        Ok(match t {
            Some(parser::Type::Int) =>  Type::Int,
            Some(parser::Type::Float) => Type::Float,
            Some(parser::Type::Bool) => Type::Bool,
            Some(parser::Type::Str) => Type::Str,
            Some(parser::Type::None) => Type::Null,
            Some(parser::Type::Arr(t)) => Type::Arr(Box::new(self.parse_type(*t)?)),
            Some(parser::Type::Option(tps)) => {
                let mapped: Result<Vec<Type>,CheckerError> = tps.iter().map(|t| self.parse_type(Some(t.clone()))).collect();
                Type::Option(mapped?)
            }
            Some(parser::Type::Custom(f)) => Type::Custom(f),
            None => Type::Unknown,
        })
    }

    fn pointer(&self,t:&str) -> Result<PointerItem,CheckerError> {
        Ok(PointerItem {index:self.current,item_type:t.to_string()})
    }

    fn next(&mut self) {self.current+=1}
}