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
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;
use std::any::Any;
use std::fs::File;
use std::path::Path;
use std::io::Read;

#[macro_export]
macro_rules! take2 {
    ($itr:ident) => (($itr.next(), $itr.next()))
}

#[macro_export]
macro_rules! take3 {
    ($itr:ident) => (($itr.next(), $itr.next(), $itr.next()))
}

mod tokenizer;
use tokenizer::*;

pub mod builtins;
use builtins::*;

pub mod symbols;
use symbols::{Atom, AtomRegistry, Symbol, Namespace};

pub mod lambda;
use lambda::{EvalOption, Func, UserFunc};

/// A Lisp object
///
/// Each item in this enumeration should have a single member which is the
/// actual data. If you copy this data structure, you are also copying inner
/// data, unless it is a Ref or Ext. However this structure is usually passed
/// around as a plain (native Rust) reference within the lisp interpreter.
///
/// When a Lisp object needs to be (potentially) referred to in more than one
/// place, then it can be wrapped in an LispObj::Ref. Currently this happens
/// when an object is assigned to a variable. I haven't given much time to
/// studying the semantics of setting and getting elisp memory locations
/// (setf), so the way things currently work is probably wrong. On the other
/// hand using an enum of concrete values, then switching to a reference when
/// needed seems like a reasonable way to handle data efficiently in an
/// interpreter. Although resolving LispObj::Refs is currently a mess.
#[derive(Debug, Clone)]
pub enum LispObj {
    /// Integer
    Int(i32),
    /// String
    Str(String),
    /// An Atom
    Atm(Atom),
    /// A Symbol
    Sym(Symbol),
    /// S-Expression or list
    Sxp(Sexp),
    /// Function defined by the user
    ///
    /// The object UserFunc is bulking up the size of LispObj
    Lambda(UserFunc),
    /// A reference to an object
    Ref(LispObjRef),
    /// A reference to a native Rust structure
    Ext(External),
    /// A reference to a native function
    ExtFun(ExternalFun),
}

pub type LispObjRef = Rc<RefCell<LispObj>>;
pub type External = Rc<RefCell<LispForm>>;
pub type ExternalFun = Rc<Func>;

macro_rules! gen_to_vals {
    ( $( $fn:ident, $inner:ident, $type:ident );+ ) => ($(
        fn $fn(&self) -> Result<&$type, String> {
            if let &LispObj::$inner(ref val) = self {
                Ok(val)
            } else {
                Err(format!(concat!("Expected LispObj:", stringify!($inner), ", but instead found {:?}"),
                            self))
            }
        }
    )+)
}

macro_rules! gen_is_x {
    ( $( $fn:ident, $inner:ident );+ ) => ($(
        pub fn $fn(&self) -> bool {
            if let &LispObj::$inner(_) = self {
                true
            } else {
                false
            }
        }
    )+)
}

#[allow(dead_code)]
impl LispObj {

    gen_to_vals!{int_val, Int, i32;
                 str_val, Str, String;
                 atm_val, Atm, Atom;
                 sym_val, Sym, Symbol;
                 sxp_val, Sxp, Sexp;
                 lam_val, Lambda, UserFunc;
                 ref_val, Ref, LispObjRef;
                 ext_val, Ext, External}

    gen_is_x!{is_int, Int;
              is_str, Str;
              is_atm, Atm;
              is_sym, Sym;
              is_sxp, Sxp;
              is_lam, Lambda;
              is_ref, Ref;
              is_ext, Ext}

    fn ref_sxp(&mut self) -> &mut Sexp {
        if let &mut LispObj::Sxp(ref mut sxp) = self {
            sxp
        } else {
            panic!("Not an Sexp");
        }
    }

    pub fn into_ref(self) -> LispObjRef {
        Rc::new(RefCell::new(self))
    }

    pub fn nil() -> LispObj {
        LispObj::atm(symbols::NIL)
    }

    pub fn t() -> LispObj {
        LispObj::atm(symbols::T)
    }

    pub fn atm(name: Atom) -> LispObj {
        LispObj::Atm(name)
    }

    pub fn sym(name: Atom) -> LispObj {
        LispObj::Sym(Symbol::new(name))
    }

    pub fn str(strng: &str) -> LispObj {
        LispObj::Str(strng.to_owned())
    }

    pub fn pair(a: LispObj, b: LispObj) -> LispObj {
        LispObj::Sxp(Sexp::from(&[a, b]))
    }

    pub fn list_from(items: &[LispObj]) -> LispObj {
        LispObj::Sxp(Sexp::from(items))
    }

    fn is_nil(&self) -> bool {
        match self {
            &LispObj::Atm(a) if a == symbols::NIL => true,
            &LispObj::Sxp(ref sxp) if sxp.lst.len() == 0 => true,
            _ => false,
        }
    }

    pub fn extern_fun<F: 'static + Func>(fun: F) -> LispObj {
        LispObj::ExtFun(Rc::new(fun))
    }
}

// This should probably not be used by lisp builtins. Emacs has at least 3
// equality operators: ==, eq, equals. This could be used for eq or equals,
// but this may conflict with its use by the Rust standard library
// (e.g. in assert_eq! or HashMap)
impl std::cmp::PartialEq for LispObj {
    fn eq(&self, other: &LispObj) -> bool {
        macro_rules! exact_eq {
            ($var:ident, $type:ident) => (
                 if let &LispObj::$type(ref b) = other {
                    $var == b
                } else {
                    false
                }
            )
        }

        match self {
            &LispObj::Int(ref i) => exact_eq!(i, Int),
            &LispObj::Str(ref s) => exact_eq!(s, Str),
            &LispObj::Sym(ref s) => exact_eq!(s, Sym),
            &LispObj::Sxp(ref s) => exact_eq!(s, Sxp),
            _ => panic!("Equality not implemented for {:?}", self),
        }
    }
}

impl fmt::Display for LispObj {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LispObj::Int(i) => write!(f, "{}", i),
            &LispObj::Str(ref s) => write!(f, "\"{}\"", s),
            &LispObj::Atm(a) => write!(f, "{}", a),
            &LispObj::Sym(ref s) => write!(f, "{:?}", s),
            &LispObj::Sxp(ref sxp) => write!(f, "{}", sxp),
            &LispObj::Lambda(ref fun) => write!(f, "{}", fun),
            &LispObj::Ref(ref iref) => write!(f, "{}", &iref.borrow()),
            &LispObj::Ext(ref ext) => {
                let ext = ext.borrow();
                match ext.to_lisp() {
                    Ok(l) => write!(f, "{}", l),
                    Err(_) => write!(f, "#<{}>", ext.rust_name())
                }
            },
            &LispObj::ExtFun(ref fun) => {
                write!(f, "{:?}", fun)
            }
        }
    }
}

/// S-Expression, list or vector
///
/// Usually a list in Lisp is a string of cons cells (linked-list). Here we
/// are using a Rust vector and emulating cons cells. This is mainly because
/// it allows me to reuse the Rust Vec/slice types and associated iterator
/// functions. It is probably also more efficient for larger lists and the
/// same for smaller.
#[derive(Debug, Clone, PartialEq)]
pub struct Sexp {
    delim: char,
    lst: Vec<LispObj>,
}

impl Sexp {
    pub fn root(func: Atom) -> Sexp {
        Sexp {
            delim: 'R',
            lst: vec![LispObj::Atm(func)],
        }
    }

    pub fn nil() -> Sexp {
        Sexp {
            delim: '(',
            lst: Vec::new(),
        }
    }

    pub fn new(delim: char) -> Sexp {
        Sexp {
            delim: delim,
            lst: Vec::new(),
        }
    }

    pub fn from(lst: &[LispObj]) -> Sexp {
        Sexp {
            delim: '(',
            lst: Vec::from(lst),
        }
    }

    pub fn vec_from(lst: &[LispObj]) -> Sexp {
        Sexp {
            delim: '[',
            lst: Vec::from(lst),
        }
    }

    pub fn push(&mut self, child: LispObj) {
        self.lst.push(child);
    }

    pub fn car(&self) -> LispObj {
        match self.lst.first() {
            Some(car) => car.clone(),
            None => LispObj::nil(),
        }
    }

    pub fn cdr(&self) -> LispObj {
        match self.lst.split_first() {
            Some((_, rest)) => LispObj::Sxp(Sexp::from(rest)),
            _ => LispObj::nil(),
        }
    }

    fn new_inner_sxp(&mut self, delim: char) -> &mut Sexp {
        self.lst.push(LispObj::Sxp(Sexp::new(delim)));
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

/// Types which can be converted to and from Lisp
pub trait LispForm: fmt::Debug {
    fn rust_name(&self) -> &'static str;
    fn lisp_name(&self) -> &'static str;

    fn to_lisp(&self) -> Result<LispObj, String> {
        Err(format!("Type {} ({}) can not be converted to Lisp",
                    self.rust_name(), self.lisp_name()))
    }

    fn from_lisp(&self, LispObj) -> Result<LispObj, String> {
        Err(format!("Type {} ({}) can not be created from Lisp",
                    self.rust_name(), self.lisp_name()))
    }

    fn as_any(&mut self) -> &mut Any;
}

/// Try to downcast an External trait to its concrete type
///
/// This is necessary for interacting with arbitrary structs shared between
/// Lisp and Rust.
#[macro_export]
macro_rules! with_downcast {
    ($value:ident, $as:ident; $do:block) => (
        if let &LispObj::Ext(ref ext) = $value {
            let ext = &mut *ext.borrow_mut();
            let lname = ext.lisp_name();
            let rname = ext.rust_name();

            if let Some($value) = ext.as_any().downcast_mut::<$as>() {
                Ok($do)
            } else {
                Err(format!("Downcast did not expect {} ({})", lname, rname))
            }
        } else {
            Err(format!("Only external objects can be downcast, not: {}", $value))
        }
    )
}

/// The Lisp interpreter
///
/// Possibly also the compiler in the future. Currently this just executes the
/// AST as is.
pub struct Lsp {
    pub globals: Namespace,
    pub locals: Vec<Namespace>,
    atoms: AtomRegistry,
}

impl Tokenizer for Lsp {
    fn atoms(&mut self) -> &mut AtomRegistry {
        &mut self.atoms
    }
}

impl Lsp {
    pub fn new() -> Lsp {
        let mut ar = AtomRegistry::with_capacity(1000);
        let mut g = Namespace::new();

        macro_rules! register_ext_funcs {
            ( $($builtin:ident),+ ) => { $(
                let fun = $builtin::new_ar(&mut ar);
                g.intern(Symbol::with_ext_fun(fun.name(), fun));
            )+ }
        }

        register_ext_funcs!(
            PlusBuiltin,
            MinusBuiltin,
            QuoteBuiltin,
            InteractiveBuiltin,
            PrintBuiltin,
            ExitBuiltin,
            PrognBuiltin,
            IfBuiltin,
            EqBuiltin,
            ConsBuiltin,
            CarBuiltin,
            CdrBuiltin,
            ListpBuiltin,
            LoadBuiltin
        );

        g.intern(Symbol::with_val(symbols::LOAD_PATH,
                                  LispObj::list_from(&[LispObj::str("lisp")])));

        Lsp {
            globals: g,
            locals: Vec::new(),
            atoms: ar,
        }
    }

    pub fn atomize(&mut self, name: &str) -> Atom {
        self.atoms.atomize(name)
    }

    pub fn stringify(&self, atom: Atom) -> &str {
        self.atoms.stringify(atom)
    }

    pub fn set_global(&mut self, name: &str, value: LispObj) {
        let name = self.atoms.atomize(name);
        self.globals.intern(Symbol::with_val(name, value));
    }

    pub fn read(&mut self, input: &String) -> Result<Sexp, String> {
        match self.tokenize(input) {
            Ok(toks) => {
                //Iterator
                let mut itr = toks.iter().peekable();
                //AST root
                let mut tree = Sexp::root(self.atoms.atomize("progn"));
                //Are we inside a (quote ...)
                let mut quot = false;

                {
                    let mut anc = vec![&mut tree];

                    while let Some(t) = itr.next() {
                        let cur = anc.pop().unwrap();

                        match t {
                            &Token::Lbr(c) => unsafe {
                                let cur = cur as *mut Sexp;
                                let nsxp = (&mut *cur).new_inner_sxp(c);
                                // Quote only holds one item, so don't add it back to the stack
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
                            &Token::Atm(a) => {
                                cur.push(LispObj::atm(a));
                                if quot {
                                    quot = false;
                                } else {
                                    anc.push(cur);
                                }
                            },
                            &Token::Num(ref n) => {
                                cur.push(LispObj::Int(n.significand.clone()));
                                if quot {
                                    quot = false;
                                } else {
                                    anc.push(cur);
                                }
                            },
                            &Token::Str(ref s) => {
                                cur.push(LispObj::str(s));
                                if quot {
                                    quot = false;
                                } else {
                                    anc.push(cur);
                                }
                            },
                            &Token::Qot => unsafe {
                                let cur = cur as *mut Sexp;
                                let nsxp = (&mut *cur).new_inner_sxp('(');
                                nsxp.push(LispObj::atm(symbols::QUOTE));
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
    fn eval_atm_val(&self, atm: Atom) -> Result<LispObj, String> {
        for ns in self.locals.iter().rev() {
            if let Some(var) = ns.get_val(atm) {
                return Ok(var);
            }
        }

        if let Some(var) = self.globals.get_val(atm) {
            Ok(var)
        } else {
            Err(format!("No variable named {}", self.atoms.stringify(atm)))
        }
    }

    #[inline]
    fn eval_sym_val(&self, sym: &Symbol) -> Result<LispObj, String> {
        if let Some(var) = sym.get_val() {
            Ok(var)
        } else {
            Err(format!("No variable named: {}", self.atoms.stringify(sym.name)))
        }
    }

    #[inline]
    fn eval_ref(&mut self, iref: &LispObjRef) -> Result<LispObj, String> {
        self.eval_inner(&iref.borrow())
    }

    #[inline]
    pub fn eval_inner(&mut self, ast: &LispObj) -> Result<LispObj, String> {
        match ast {
            &LispObj::Atm(a) => self.eval_atm_val(a),
            &LispObj::Sym(ref s) => self.eval_sym_val(s),
            &LispObj::Sxp(ref sxp) => self.eval(sxp),
            &LispObj::Ref(ref iref) => self.eval_ref(iref),
            _ => Ok(ast.clone()),
        }
    }

    #[inline]
    fn eval_rest(&mut self, args: &mut Iter<LispObj>) -> Result<Vec<LispObj>, String> {
        args.map( |arg| self.eval_inner(arg) ).collect()
    }

    #[inline]
    fn apply(&mut self, fun: &Func, args: &mut Iter<LispObj>) -> Result<LispObj, String> {
        match fun.eval_args() {
            EvalOption::Evaluated => {
                let ev_args = self.eval_rest(args)?;
                fun.call(self, &mut ev_args.iter())
            },
            EvalOption::Unevaluated => fun.call(self, args),
        }
    }

    #[inline]
    fn eval_fn(&mut self, fun: &LispObj, args: &mut Iter<LispObj>) -> Result<LispObj, String> {
        use std::borrow::Borrow;

        let fun = match fun {
            &LispObj::Lambda(ref lmbda) => lmbda,
            &LispObj::ExtFun(ref extf) => Rc::borrow(extf) as &Func,
            obj => return Err(format!("Not a function: {}", obj)),
        };
        self.apply(fun, args)
    }

    #[inline]
    fn eval_atm_fn(&mut self, atm: Atom, args: &mut Iter<LispObj>) -> Result<LispObj, String> {
        self.locals.iter().rev()
            .map( |ns| ns.get_fun(atm) )
            .find( |fun| fun.is_some() )
            .unwrap_or_else( || self.globals.get_fun(atm) )
            .map_or(Err(format!("Unrecognised function: {}", atm)),
                    |fun| self.eval_fn(&fun, args) )
    }

    pub fn eval_primitive(&mut self, ast: &Sexp) -> Result<LispObj, String> {
        let mut itr = ast.lst.iter();

        if let Some(name) = itr.next() {
            match name {
                &LispObj::Atm(symbols::LAMBDA) => UserFunc::lambda(&mut itr),
                obj => Ok(obj.clone()),
            }
        } else {
            Ok(LispObj::nil())
        }
    }

    pub fn eval(&mut self, ast: &Sexp) -> Result<LispObj, String> {
        let mut itr = ast.lst.iter();

        if let Some(first) = itr.next() {
            match first {
                &LispObj::Atm(a) => self.eval_atm_fn(a, &mut itr),
                &LispObj::Lambda(ref fun) => self.apply(fun, &mut itr),
                &LispObj::Sxp(ref x) => match self.eval_primitive(x)? {
                    LispObj::Lambda(ref fun) => self.apply(fun, &mut itr),
                    sxp => Err(format!("Invalid as a function: {:?}", sxp)),
                },
                &LispObj::Sym(_) => Err(format!("Eval Symbol as func not implemented")),
                _ => Err(format!("Invalid as a function: {:?}", first)),
            }
        } else {
            Ok(LispObj::nil())
        }
    }

    pub fn load(&mut self, name: &str) -> Result<LispObj, String> {
        let mut src = String::new();
        {
            let lpaths = &self.globals.get_val(symbols::LOAD_PATH).unwrap();
            let lpaths = lpaths.sxp_val()?.lst.iter().map( |dir_path| -> &str {
                if let &LispObj::Str(ref dir_path) = dir_path {
                    dir_path
                } else {
                    ""
                }
            });
            let name = Path::new(name);
            let file = lpaths.map( |dir_path| {
                match Path::new(dir_path).read_dir() {
                    Ok(mut items) => {
                        items.find( |item| {
                            if let &Ok(ref item) = item {
                                if !item.file_type().unwrap().is_dir() {
                                    let path = item.path();
                                    if let (Some(stem), Some(ext)) = (path.file_stem(), path.extension()) {
                                        return stem == name && ext == "el";
                                    }
                                }
                                println!("Discarding {:?}", item);
                            } else {
                                println!("I/O ERROR: {:?}", item);
                            }
                            false
                        })
                    },
                    Err(e) => {
                        println!("I/O ERROR trying to access {:?}: {}", dir_path, e);
                        None
                    },
                }
            }).find( |load_file| load_file.is_some() );

            if file.is_none() {
                return Err(format!("Can not find {:?} in load-path", name));
            }
            let path = file.unwrap().unwrap().unwrap().path();

            match File::open(path) {
                Ok(mut fh) => {
                    if let Err(e) = fh.read_to_string(&mut src) {
                        return Err(format!("LOAD I/O ERROR: {}", e));
                    }
                },
                Err(e) => {
                    return Err(format!("LOAD FILE ERROR: {}", e));
                },
            }
        }

        let sexp = self.read(&src)?;
        self.eval(&sexp)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn reasonabl_obj_size() {
        let lisp_obj_size = size_of::<LispObj>();
        println!("size of String = {}", size_of::<String>());
        println!("size of char = {}", size_of::<char>());
        println!("size of Sexp = {}", size_of::<Sexp>());
        println!("size of UserFunc = {}", size_of::<UserFunc>());
        println!("size of LispObjRef = {}", size_of::<LispObjRef>());
        println!("size of LispObj = {}", lisp_obj_size);
        println!("size of Vec<LispObj> = {}", size_of::<Vec<LispObj>>());
        assert!(lisp_obj_size <= 64);
    }

    #[test]
    fn lambda_primitive_no_args() {
        let mut lsp = Lsp::new();
        let src = "((lambda () (+ 1 2)))".to_owned();

        let ast = &lsp.read(&src).unwrap();
        assert_eq!(lsp.eval(ast), Ok(LispObj::Int(3)));
    }

    #[test]
    fn lambda_primitive() {
        let mut lsp = Lsp::new();
        let src = "((lambda (a b) (+ a b)) 1 2)".to_owned();

        let ast = &lsp.read(&src).unwrap();
        assert_eq!(lsp.eval(ast), Ok(LispObj::Int(3)));
    }
}
