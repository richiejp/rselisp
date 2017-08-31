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

use std::slice::Iter;
use std::collections::HashMap;
use std::rc::Rc;

mod tokenizer;
use tokenizer::*;

#[derive(Debug, Clone)]
pub enum Inner {
    Int(i32),
    Str(String),
    Sym(String),
    Sxp(Sexp),
    Lambda(Box<UserFunc>),
}

impl Inner {
    fn ref_sxp(&mut self) -> &mut Sexp {
        if let &mut Inner::Sxp(ref mut sxp) = self {
            sxp
        } else {
            panic!("Not an Sexp");
        }
    }
}

#[derive(Debug, Clone)]
pub struct Sexp {
    delim: char,
    lst: Vec<Inner>,
}

impl Sexp {
    fn root() -> Sexp {
        Sexp {
            delim: 'R',
            lst: Vec::new(),
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

    fn new_str(s: &str) -> Sexp {
        Sexp {
            delim: '(',
            lst: vec![Inner::Str(s.to_owned())]
        }
    }
    
    fn push(&mut self, child: Inner) {
        self.lst.push(child);
    }

    fn iter(&self) -> Iter<Inner> {
        self.lst.iter()
    }

    fn new_inner_sxp(&mut self, delim: char) -> &mut Sexp {
        self.lst.push(Inner::Sxp(Sexp::new(delim)));
        self.lst.last_mut().unwrap().ref_sxp()
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

#[derive(Clone, Debug)]
pub struct UserFunc {
    name: String,
    args: Vec<ArgSpec>,
    body: Inner,
}

impl UserFunc {
    fn new(name: String, args: Vec<ArgSpec>, body: Inner) -> UserFunc {
        UserFunc {
            name: name,
            args: args,
            body: body,
        }
    }
}

impl Func for UserFunc {
    fn eval_args(&self) -> EvalOption { EvalOption::Evaluated }
    fn name(&self) -> &str { &self.name }

    fn call(&self, lsp: &mut Lsp, args: &mut Iter<Inner>) -> Result<Inner, String> {
        lsp.eval_inner(&self.body)
    }
}

impl Func for Box<UserFunc> {
    fn eval_args(&self) -> EvalOption { EvalOption::Evaluated }
    fn name(&self) -> &str { &self.name }

    fn call(&self, lsp: &mut Lsp, args: &mut Iter<Inner>) -> Result<Inner, String> {
        lsp.eval_inner(&self.body)
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

def_builtin! { "-", MinusBuiltin, Evaluated, lsp, args; {
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

def_builtin! { "+", PlusBuiltin, Evaluated, lsp, args; {
    let mut res = 0;

    while let Some(arg) = args.next() {
        match arg {
            &Inner::Int(i) => res += i,
            _ => return Err(format!("Expected int, but found {:?}", arg)),
        };
    }

    Ok(Inner::Int(res))
}}

def_builtin! { "quote", QuoteBuiltin, Unevaluated, lsp, args; {
    let argt = (args.next(), args.next());
    match argt {
        (Some(arg), None) => Ok(arg.clone()),
        _ => Err(format!("Wrong number of arguments; quote only accepts one")),
    }
}}

def_builtin! { "lambda", LambdaBuiltin, Unevaluated, lsp, args; {
    match (args.next(), args.next()) {
        (Some(&Inner::Sxp(_)), Some(body)) => {
            Ok(Inner::Lambda(Box::new(
                UserFunc::new("".to_owned(), Vec::new(), body.clone())
            )))
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

struct Namespace {
    funcs: HashMap<String, Rc<Func>>,
    vars: HashMap<String, Rc<Inner>>,
}

impl Namespace {
    fn new() -> Namespace {
        Namespace {
            funcs: HashMap::new(),
            vars: HashMap::new(),
        }
    }

    fn reg_fn<F: 'static + Func>(&mut self, fun: F) {
        self.funcs.insert(fun.name().to_owned(), Rc::new(fun));
    }

    fn reg_var(&mut self, name: String, var: Inner) {
        self.vars.insert(name, Rc::new(var));
    }
}

pub struct Lsp {
    globals: Namespace,
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

        g.reg_var("t".to_owned(), Inner::Sym("t".to_owned()));
        g.reg_var("nil".to_owned(), Inner::Sxp(Sexp::nil()));
        
        Lsp {
            globals: g,
        }
    }
    
    pub fn read(&self, input: &String) -> Result<Sexp, String> {
        match self.tokenize(input) {
            Ok(toks) => {
                //Iterator
                let mut itr = toks.iter().peekable();
                //AST root
                let mut tree = Sexp::root();

                {
                    let mut anc = vec![&mut tree];

                    while let Some(t) = itr.next() {
                        let cur = anc.pop().unwrap();

                        match t {
                            &Token::Lbr(c) => unsafe {
                                let cur = cur as *mut Sexp;
                                let nsxp = (&mut *cur).new_inner_sxp(c);
                                anc.push(&mut *cur);
                                anc.push(nsxp);
                            },
                            &Token::Rbr(c) => {
                                if cur.delim == 'R' {
                                    return Err(format!("There are more '{}' than '{}'", c, Lsp::inv_brk(c)));
                                } else if cur.delim != Lsp::inv_brk(c) {
                                    return Err(format!("Mismatch '{}' with '{}'", cur.delim, c));
                                }
                            },
                            &Token::Atm(ref s) => {
                                if let Ok(i) = i32::from_str_radix(s, 10) {
                                    cur.push(Inner::Int(i));
                                } else {
                                    cur.push(Inner::Sym(s.to_owned()));
                                }
                                anc.push(cur);
                            },
                            &Token::Str(ref s) => {
                                cur.push(Inner::Str(s.to_owned()));
                                anc.push(cur);
                            },
                            &Token::Qot => return Err(format!("Quotes not implemented")),
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
        if let Some(val) = self.globals.vars.get(sym) {
            Ok(Inner::clone(val))
        } else {
            Err(format!("No variable named {}", sym))
        }
    }

    #[inline]
    fn eval_inner(&mut self, ast: &Inner) -> Result<Inner, String> {
        match ast {
            &Inner::Sym(ref s) => self.eval_sym(s),
            &Inner::Sxp(ref sxp) => self.eval(sxp),
            _ => Ok(ast.clone()),
        }
    }

    #[inline]
    fn eval_rest(&mut self, args: &mut Iter<Inner>) -> Result<Sexp, String> {
        let mut res = Sexp::nil();

        while let Some(arg) = args.next() {
            res.push(self.eval_inner(arg)?);
        }

        Ok(res)
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
