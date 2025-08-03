

use std::{collections::HashMap, thread::Builder};

use inkwell::{context::Context, types::{BasicType, BasicTypeEnum, FunctionType, StructType}, AddressSpace};
use crate::checker::{Command, Comp, Enum, Field, Func, Type, Value,Var};

pub struct Compiler {
    pub fields: HashMap<String,Field>,
    pub glob_funcs: HashMap<String,Func>,
    pub enums: HashMap<String,Enum>,
    pub context: Context
}

impl Compiler {
    pub fn run(&self) {
        let mut module = self.context.create_module("Main");
        let builder = self.context.create_builder();

        for (name,field) in self.fields.clone() {
            let f = self.context.opaque_struct_type(&name);
            let mut types = Vec::new();
            for (_,var) in self.get_field_values(&field) {
                types.push(self.parse_type(&var.var_type).unwrap());
            }
            f.set_body(types.as_slice(), false);
            for (name,fnc) in self.get_field_methods(&field) {
                let function = module.add_function(&&name,self.method(&fnc),None);
                
            }
        }

        module.print_to_stderr();
    }

    fn add_body(&self,bldr:&inkwell::builder::Builder,func:inkwell::values::FunctionValue,f:&Func) {
        for (p,(n,_)) in func.get_params().iter().zip(f.args.iter()) {
            p.set_name(&n);
        }

        let mut symbols = HashMap::new();
        let entryb = self.context.append_basic_block(func, "entry");
        bldr.position_at_end(entryb);

        for c in f.body.iter() {
            match c {
                Command::Declare { name, var_type, value } => {
                    let alloca = bldr.build_alloca(self.parse_type(var_type).unwrap(), &name)?;
                    symbols.insert(name, alloca);
                }
                Command::Assign { target, value } => {
                
                }
            }
        }
    }

    fn expr()

    fn method(&self,func:&Func) -> FunctionType {
        match self.parse_type(&func.ret) {
            Some(rettype) => {
                let mut params = Vec::new();
                for (pn,p) in &func.args {
                    params.push(self.parse_type(&p.var_type).unwrap().into());
                }
                rettype.fn_type(params.as_slice(), false)
            },
            None => {
                let mut params = Vec::new();
                for (pn,p) in &func.args {
                    params.push(self.parse_type(&p.var_type).unwrap().into());
                }
                self.context.void_type().fn_type(params.as_slice(), false)
            }
        }
    }

    fn parse_type(&self,t:&Type) -> Option<inkwell::types::BasicTypeEnum> {
        match t {
            Type::Int => Some(self.context.i32_type().into()),
            Type::Float => Some(self.context.f32_type().into()),
            Type::Bool => Some(self.context.bool_type().into()),
            Type::Null => None,
            Type::Unknown => None,
            Type::Custom(_) => Some(self.context.struct_type(&[
                self.context.ptr_type(AddressSpace::from(0)).into(),
                self.context.ptr_type(AddressSpace::from(0)).into(),
            ],false).into()),
            _ => Some(self.context.struct_type(&[
                self.context.ptr_type(AddressSpace::from(0)).into(),
                self.context.i32_type().into()
            ], false).into())
        }
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
}
