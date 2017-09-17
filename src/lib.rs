// Copyright (C) 2017 Richard Palethorpe <richiejp@f-m.fm>

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

extern crate fnv;

use std::slice::Iter;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

use fnv::FnvHashMap;

mod tokenizer;
use tokenizer::*;

#[derive(Debug, Clone)]
pub enum Inner {
    Int(i32),
    Str(String),
    Sym(String),
    Sxp(Sexp),
    Lambda(UserFunc),
    Ref(InnerRef),
}

type InnerRef = Rc<RefCell<Inner>>;

impl Inner {
    fn ref_sxp(&mut self) -> &mut Sexp {
        if let &mut Inner::Sxp(ref mut sxp) = self {
            sxp
        } else {
            panic!("Not an Sexp");
        }
    }

    fn into_ref(self) -> InnerRef {
        Rc::new(RefCell::new(self))
    }

    fn nil() -> Inner {
        Inner::Sxp(Sexp::nil())
    }

    fn t() -> Inner {
        Inner::Sym("t".to_owned())
    }

    fn is_nil(&self) -> bool {
        match self {
            &Inner::Sym(ref s) if s == "nil" => true,
            &Inner::Sxp(ref sxp) if sxp.lst.len() == 0 => true,
            _ => false,
        }
    }
}

impl std::cmp::PartialEq for Inner {
    fn eq(&self, other: &Inner) -> bool {
        match self {
            &Inner::Int(i) => {
                if let &Inner::Int(oi) = other {
                    i == oi
                } else {
                    false
                }
            },
            _ => panic!("Only equality of Inner::Int implemented"),
        }
    }
}

impl fmt::Display for Inner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Inner::Int(i) => write!(f, "{}", i),
            &Inner::Str(ref s) => write!(f, "\"{}\"", s),
            &Inner::Sym(ref s) => write!(f, "{}", s),
            &Inner::Sxp(ref sxp) => write!(f, "{}", sxp),
            &Inner::Lambda(ref fun) => write!(f, "{}", fun),
            &Inner::Ref(ref iref) => write!(f, "{}", &iref.borrow()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Sexp {
    delim: char,
    lst: Vec<Inner>,
}

impl Sexp {
    pub fn root(func: String) -> Sexp {
        Sexp {
            delim: 'R',
            lst: vec![Inner::Sym(func)],
        }
    }

    fn nil() -> Sexp {
        Sexp {
            delim: '(',
            lst: Vec::new(),
        }
    }
    
    fn new(delim: char) -> Sexp {
        Sexp {
            delim: delim,
            lst: Vec::new(),
        }
    }

    fn push(&mut self, child: Inner) {
        self.lst.push(child);
    }

    fn new_inner_sxp(&mut self, delim: char) -> &mut Sexp {
        self.lst.push(Inner::Sxp(Sexp::new(delim)));
        self.lst.last_mut().unwrap().ref_sxp()
    }
}

fn fmt_iter<E, F>(brk: char, itr: &mut Iter<E>, f: &mut F) -> fmt::Result
    where E: fmt::Display, F: fmt::Write
{
    if let Some(first) = itr.next() {
        write!(f, "{}{}", brk, first)?;
        for elt in itr {
            write!(f, " {}", elt)?;
        }
        write!(f, "{}", Lsp::inv_brk(brk))
    } else {
        write!(f, "nil")
    }
}

impl fmt::Display for Sexp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            fmt_iter('(', &mut self.lst.iter(), f)
    }
}

#[derive(Clone)]
enum EvalOption {
    Evaluated,
    Unevaluated,
}

trait Func {
    fn eval_args(&self) -> EvalOption;
    fn name(&self) -> &str;
    fn call(&self, &mut Lsp, &mut Iter<Inner>) -> Result<Inner, String>;
}

#[derive(Clone, Debug)]
pub struct ArgSpec {
    name: String,
}

impl ArgSpec {
    fn new(name: String) -> ArgSpec {
        ArgSpec {
            name: name,
        }
    }
}

impl fmt::Display for ArgSpec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.name)
    }
}

#[derive(Clone, Debug)]
pub struct ArgSpecs(Vec<ArgSpec>);

impl std::ops::Deref for ArgSpecs {
    type Target = Vec<ArgSpec>;

    fn deref(&self) -> &Vec<ArgSpec> {
        &self.0
    }
}

impl fmt::Display for ArgSpecs {
fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_iter('[', &mut self.iter(), f)
    }
}

#[derive(Clone, Debug)]
pub struct UserFunc {
    name: String,
    args: ArgSpecs,
    body: InnerRef,
}

impl UserFunc {
    fn new(name: String, args: Vec<ArgSpec>, body: InnerRef) -> UserFunc {
        UserFunc {
            name: name,
            args: ArgSpecs(args),
            body: body,
        }
    }
}

impl Func for UserFunc {
    fn eval_args(&self) -> EvalOption { EvalOption::Evaluated }
    fn name(&self) -> &str { &self.name }

    fn call(&self, lsp: &mut Lsp, args: &mut Iter<Inner>) -> Result<Inner, String> {
        let mut ns = Namespace::new();
        for spec in self.args.iter() {
            if let Some(arg) = args.next() {
                ns.reg_var(spec.name.clone(), arg);
            } else {
                return Err(format!("'{}' expected '{}' argument", self.name(), spec.name));
            }
        }
        lsp.locals.push(ns);
        let ret = lsp.eval_ref(&self.body);
        lsp.locals.pop();
        ret
    }
}

impl fmt::Display for UserFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "('{} . (lambda {} {}))", &self.name, &self.args, Inner::Ref(self.body.clone()))
    }
}

macro_rules! def_builtin {
    ($name:expr, $rname:ident, $evaled:ident, $lsp:ident, $args:ident; $fn_body:block ) => (
        #[derive(Clone)]
        struct $rname { }
        impl Func for $rname {
            fn eval_args(&self) -> EvalOption {
                EvalOption::$evaled
            }

            fn name(&self) -> &'static str {
                $name
            }

            fn call(&self, $lsp: &mut Lsp, $args: &mut Iter<Inner>) -> Result<Inner, String> {
                $fn_body
            }
        }
    )
}

def_builtin! { "-", MinusBuiltin, Evaluated, _lsp, args; {
    let mut res = 0;

    if let Some(arg) = args.next() {
        match arg {
            &Inner::Int(i) => res = i,
            _ => return Err(format!("Expected int, but found {:?}", arg)),
        }
    }

    if let Some(arg) = args.next() {
        match arg {
            &Inner::Int(i) => res -= i,
            _ => return Err(format!("Expected int, but found {:?}", arg)),
        }
    } else {
        res = -res;
    }

    while let Some(arg) = args.next() {
        match arg {
            &Inner::Int(i) => res -= i,
            _ => return Err(format!("Expected int, but found {:?}", arg)),
        };
    }

    Ok(Inner::Int(res))
}}

def_builtin! { "+", PlusBuiltin, Evaluated, _lsp, args; {
    let mut res = 0;

    while let Some(arg) = args.next() {
        match arg {
            &Inner::Int(i) => res += i,
            _ => return Err(format!("Expected int, but found {:?}", arg)),
        };
    }

    Ok(Inner::Int(res))
}}

def_builtin! { "quote", QuoteBuiltin, Unevaluated, _lsp, args; {
    let argt = (args.next(), args.next());
    match argt {
        (Some(arg), None) => Ok(arg.clone()),
        _ => Err(format!("Wrong number of arguments; quote only accepts one")),
    }
}}

def_builtin! { "lambda", LambdaBuiltin, Unevaluated, _lsp, args; {
    match (args.next(), args.next()) {
        (Some(&Inner::Sxp(ref args_sxp)), Some(body)) => {
            let largs: Result<Vec<ArgSpec>, String> = args_sxp.lst.iter().map(
                |arg| -> Result<ArgSpec, String> {
                    match arg {
                        &Inner::Sym(ref name) => Ok(ArgSpec::new(name.to_owned())),
                        _ => Err(format!("Lambda arguments must be symbols")),
                    }
                }
            ).collect();

            Ok(Inner::Lambda(
                UserFunc::new("".to_owned(), largs?, match body {
                    &Inner::Ref(ref iref) => iref.clone(),
                    _ => body.clone().into_ref(),
                })
            ))
        },
        _ => Err(format!("(lambda ([args]) [body])")),
    }
}}

def_builtin! { "defalias", DefaliasBuiltin, Evaluated, lsp, args; {
    let name = match args.next() {
        Some(&Inner::Sym(ref name)) => name,
        _ => return Err(format!("defalias expected symbol")),
    };

    let mut fun = match args.next() {
        Some(&Inner::Lambda(ref lmbda)) => lmbda.clone(),
        _ => return Err(format!("defalias expected lambda")),
    };

    fun.name = name.to_owned();
    lsp.globals.reg_fn(fun);
    Ok(Inner::Sym(name.to_owned()))
}}

def_builtin! { "print", PrintBuiltin, Evaluated, _lsp, args; {
    let mut s = String::new();
    let _res = fmt_iter('(', args, &mut s);
    println!("{}", &s);
    Ok(Inner::Str(s))
}}

def_builtin! { "exit", ExitBuiltin, Unevaluated, _lsp, _args; {
    Ok(Inner::Sym("exit".to_owned()))
}}

def_builtin! { "progn", PrognBuiltin, Evaluated, _lsp, args; {
    if let Some(val) = args.last() {
        Ok(val.clone())
    } else {
        Err(format!("progn requires one or more arguments"))
    }
}}

def_builtin! { "if", IfBuiltin, Unevaluated, lsp, args; {
    if let Some(cond) = args.next() {
        let then = match args.next() {
            Some(then) => then,
            None => return Err(format!("if requires a THEN argument")),
        };

        if !lsp.eval_inner(cond)?.is_nil() {
                lsp.eval_inner(then)
        } else {
            let mut ret = if let Some(lelse) = args.next() {
                lsp.eval_inner(lelse)?
            } else {
                Inner::nil()
            };

            while let Some(lelse) = args.next() {
                ret = lsp.eval_inner(lelse)?;
            }

            Ok(ret)
        }
    } else {
        Err(format!("if requires a COND argument"))
    }
}}

def_builtin! { "eq", EqBuiltin, Evaluated, _lsp, args; {
    if let (Some(left), Some(right)) = (args.next(), args.next()) {
        if left == right {
            Ok(Inner::t())
        } else {
            Ok(Inner::nil())
        }
    } else {
        Err(format!("eq requires two arguments"))
    }
}}

struct Namespace {
    funcs: FnvHashMap<String, Rc<Func>>,
    vars: FnvHashMap<String, Inner>,
}

impl Namespace {
    fn new() -> Namespace {
        Namespace {
            funcs: FnvHashMap::default(),
            vars: FnvHashMap::default(),
        }
    }

    fn reg_fn<F: 'static + Func>(&mut self, fun: F) {
        self.funcs.insert(fun.name().to_owned(), Rc::new(fun));
    }

    fn reg_var(&mut self, name: String, var: &Inner) {
        self.vars.insert(name, match var {
            &Inner::Sxp(_) => Inner::Ref(var.clone().into_ref()),
            _ => var.clone(),
        });
    }
}

pub struct Lsp {
    globals: Namespace,
    locals: Vec<Namespace>,
}

impl Tokenizer for Lsp { }

impl Lsp {
    pub fn new() -> Lsp {
        let mut g = Namespace::new();

        g.reg_fn(PlusBuiltin { });
        g.reg_fn(MinusBuiltin { });
        g.reg_fn(QuoteBuiltin { });
        g.reg_fn(LambdaBuiltin { });
        g.reg_fn(DefaliasBuiltin { });
        g.reg_fn(PrintBuiltin { });
        g.reg_fn(ExitBuiltin { });
        g.reg_fn(PrognBuiltin { });
        g.reg_fn(IfBuiltin { });
        g.reg_fn(EqBuiltin { });

        g.reg_var("t".to_owned(), &Inner::t());
        g.reg_var("nil".to_owned(), &Inner::nil());
        
        Lsp {
            globals: g,
            locals: Vec::new(),
        }
    }
    
    pub fn read(&self, input: &String) -> Result<Sexp, String> {
        match self.tokenize(input) {
            Ok(toks) => {
                //Iterator
                let mut itr = toks.iter().peekable();
                //AST root
                let mut tree = Sexp::root("progn".to_owned());
                let mut quot = false;

                {
                    let mut anc = vec![&mut tree];

                    while let Some(t) = itr.next() {
                        let cur = anc.pop().unwrap();

                        match t {
                            &Token::Lbr(c) => unsafe {
                                let cur = cur as *mut Sexp;
                                let nsxp = (&mut *cur).new_inner_sxp(c);
                                if quot {
                                    quot = false;
                                } else {
                                    anc.push(&mut *cur);
                                }
                                anc.push(nsxp);
                            },
                            &Token::Rbr(c) => {
                                if cur.delim == 'R' {
                                    return Err(format!("There are more '{}' than '{}'", c, Lsp::inv_brk(c)));
                                } else if cur.delim != Lsp::inv_brk(c) {
                                    return Err(format!("Mismatch '{}' with '{}'", cur.delim, c));
                                } else if quot {
                                    return Err(format!("Can't quote closing delimiter '{}'", c));
                                }
                            },
                            &Token::Atm(ref s) => {
                                if let Ok(i) = i32::from_str_radix(s, 10) {
                                    cur.push(Inner::Int(i));
                                } else {
                                    cur.push(Inner::Sym(s.to_owned()));
                                }
                                if quot {
                                    quot = false;
                                } else {
                                    anc.push(cur);
                                }
                            },
                            &Token::Str(ref s) => {
                                cur.push(Inner::Str(s.to_owned()));
                                if quot {
                                    quot = false;
                                } else {
                                    anc.push(cur);
                                }
                            },
                            &Token::Qot => unsafe {
                                let cur = cur as *mut Sexp;
                                let nsxp = (&mut *cur).new_inner_sxp('(');
                                nsxp.push(Inner::Sym("quote".to_owned()));
                                if !quot {
                                    anc.push(&mut *cur);
                                }
                                anc.push(nsxp);
                                quot = true;
                            },
                            &Token::Spc => panic!("Space token not supported"),
                        }
                    }
                }
                Ok(tree)
            },
            Err(e) => Err(String::from(e)),
        }
    }

    #[inline]
    fn eval_sym(&self, sym: &str) -> Result<Inner, String> {
        for ns in self.locals.iter().rev() {
            if let Some(var) = ns.vars.get(sym) {
                return Ok(Inner::clone(var));
            }
        }

        if let Some(var) = self.globals.vars.get(sym) {
            Ok(Inner::clone(var))
        } else {
            Err(format!("No variable named {}", sym))
        }
    }

    #[inline]
    fn eval_ref(&mut self, iref: &InnerRef) -> Result<Inner, String> {
        self.eval_inner(&iref.borrow())
    }

    #[inline]
    fn eval_inner(&mut self, ast: &Inner) -> Result<Inner, String> {
        match ast {
            &Inner::Sym(ref s) => self.eval_sym(s),
            &Inner::Sxp(ref sxp) => self.eval(sxp),
            &Inner::Ref(ref iref) => self.eval_ref(iref),
            _ => Ok(ast.clone()),
        }
    }

    #[inline]
    fn eval_rest(&mut self, args: &mut Iter<Inner>) -> Result<Vec<Inner>, String> {
        args.map( |arg| self.eval_inner(arg) ).collect()
    }

    #[inline]
    fn eval_fn(&mut self, s: &str, args: &mut Iter<Inner>) -> Result<Inner, String> {
        if let Some(fun) = self.globals.funcs.get(s).cloned() {
            match fun.eval_args() {
                EvalOption::Evaluated => {
                    let ev_args = self.eval_rest(args)?;
                    fun.call(self, &mut ev_args.iter())
                },
                EvalOption::Unevaluated => fun.call(self, args),
            }
        } else {
            Err(format!("Unrecognised function: {:?}", s))
        }
    }

    pub fn eval(&mut self, ast: &Sexp) -> Result<Inner, String> {
        let mut itr = ast.lst.iter();

        if let Some(first) = itr.next() {
            if let &Inner::Sym(ref s) = first {
                return self.eval_fn(s, &mut itr);
            } else {
                return Err(format!("Invalid as a function: {:?}", first));
            }
        } else {
            Ok(Inner::Sxp(Sexp::nil()))
        }
    }
}
