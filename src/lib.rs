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

enum EvalOption {
    Evaluated,
    Unevaluated,
}

trait Func {
    fn eval_args(&self) -> EvalOption;
    fn name(&self) -> &'static str;
    fn call(&self, &mut Iter<Inner>) -> Result<Inner, String>;
}

macro_rules! def_builtin {
    ($name:expr, $rname:ident, $evaled:ident, $args:ident; $fn_body:block ) => (
        #[derive(Clone)]
        struct $rname { }
        impl Func for $rname {
            fn eval_args(&self) -> EvalOption {
                EvalOption::$evaled
            }

            fn name(&self) -> &'static str {
                $name
            }

            fn call(&self, $args: &mut Iter<Inner>) -> Result<Inner, String> {
                $fn_body
            }
        }
    )
}

def_builtin! { "-", MinusBuiltin, Evaluated, args; {
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

def_builtin! { "+", PlusBuiltin, Evaluated, args; {
    let mut res = 0;

    while let Some(arg) = args.next() {
        match arg {
            &Inner::Int(i) => res += i,
            _ => return Err(format!("Expected int, but found {:?}", arg)),
        };
    }

    Ok(Inner::Int(res))
}}

def_builtin! { "quote", QuoteBuiltin, Unevaluated, args; {
    let argt = (args.next(), args.next());
    match argt {
        (Some(arg), None) => Ok(arg.clone()),
        _ => Err(format!("Wrong number of arguments; quote only accepts one")),
    }
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

    fn reg<F: 'static + Func>(&mut self, fun: F) {
        self.funcs.insert(fun.name().to_owned(), Rc::new(fun));
    }
}

pub struct Lsp {
    globals: Namespace,
}

impl Tokenizer for Lsp { }

impl Lsp {
    pub fn new() -> Lsp {
        let mut g = Namespace::new();

        g.reg(PlusBuiltin { });
        g.reg(MinusBuiltin { });
        g.reg(QuoteBuiltin { });

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

    fn eval_inner(&mut self, ast: &Inner) -> Result<Inner, String> {
        match ast {
            &Inner::Sxp(ref sxp) => self.eval(sxp),
            _ => Ok(ast.clone()),
        }
    }

    fn eval_rest(&mut self, args: &mut Iter<Inner>) -> Result<Sexp, String> {
        let mut res = Sexp::nil();

        while let Some(arg) = args.next() {
            res.push(self.eval_inner(arg)?);
        }

        Ok(res)
    }

    fn eval_fn(&mut self, s: &str, args: &mut Iter<Inner>) -> Result<Inner, String> {
        if let Some(fun) = self.globals.funcs.get(s).cloned() {
            match fun.eval_args() {
                EvalOption::Evaluated => {
                    let ev_args = self.eval_rest(args)?;
                    fun.call(&mut ev_args.iter())
                },
                EvalOption::Unevaluated => fun.call(args),
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
