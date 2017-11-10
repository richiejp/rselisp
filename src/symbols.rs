use fnv::FnvHashMap;
use std::usize;

use super::*;

macro_rules! const_atoms {
    () => {
        NIL => "nil",
        T => "t",
        LAMBDA => "lambda",
        MACRO => "macro",
        ANONYMOUS => "#<anonymous>",
    }
}

macro_rules! gen_const_atoms {
    ( $( $const_var:ident => $name:ident ),+ ) => (
        gen_const_atoms!( $( $const_var ),+ )
    );
    ($first:ident, $($rest:ident),*) => (
        gen_const_atoms!($($rest),+ ; 0; $first = 0)
    );
    ($cur:ident, $($rest:ident),* ; $last_index: expr ; $($var:ident = $index:expr)+) => (
        gen_const_atoms!($($rest),* ; $last_index + 1; $($var = $index)* $cur = $last_index + 1)
    );
    ($cur:ident; $last_index:expr ; $($var:ident = $index:expr)+) => (
        $( pub const $var: Atom = Atom { indx: $index }; )+
            pub const $cur: Atom = Atom { indx: $last_index }
    );
}

gen_const_atoms!(const_atoms!());

// pub const NIL: Atom = Atom { indx: 0 };
// pub const T: Atom = Atom { indx: 1 };
// pub const LAMBDA: Atom = Atom { indx: 2 };
// pub const MACRO: Atom = Atom { indx: 3 };
// pub const ANONYMOUS: Atom = Atom { indx: 4 };

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Atom {
    indx: usize,
}

impl Atom {
    fn new(indx: usize) -> Atom {
        Atom { indx: indx }
    }
}

pub struct AtomRegistry {
    table: Vec<String>,
    rev_table: FnvHashMap<String, Atom>,
}

impl AtomRegistry {
    pub fn with_capacity(capacity: usize) -> AtomRegistry {
        macro_rules! atomize_const_atoms {
            ( $( $const_var:ident => $name:ident ),+ ) => (
                $( me.atomize(stringify!($name)) );+
            )
        }

        let cap = capacity + 3;
        let mut me = AtomRegistry {
            table: Vec::with_capacity(cap),
            rev_table: FnvHashMap::with_capacity_and_hasher(cap, Default::default()),
        };
        // me.atomize("nil");
        // me.atomize("t");
        // me.atomize("lambda");
        // me.atomize("macro");
        // me.atomize("#<anonymous>");
        atomize_const_atoms!(const_atoms!());
        me
    }

    pub fn atomize(&mut self, name: &str) -> Atom {
        if let Some(atom) = self.rev_table.get(name).and_then(|atm| Some(atm.clone())) {
            atom
        } else {
            self.table.push(name.into());
            let atom = Atom {
                indx: self.table.len() - 1,
            };
            self.rev_table.insert(name.into(), atom.clone());
            atom
        }
    }

    pub fn atomize_mv(&mut self, name: String) -> Atom {
        if let Some(atom) = self.rev_table.get(&name).and_then(|atm| Some(atm.clone())) {
            atom
        } else {
            self.table.push(name.clone());
            let atom = Atom {
                indx: self.table.len() - 1,
            };
            self.rev_table.insert(name, atom.clone());
            atom
        }
    }

    pub fn stringify(&self, atom: Atom) -> &str {
        &self.table[atom.indx]
    }
}

pub struct SymbolData {
    pub value: Option<LispObj>,
    pub function: Option<LispObj>,
    pub properties: Option<FnvHashMap<Atom, LispObj>>,
}

pub struct Symbol {
    pub name: Atom,
    data: Rc<RefCell<SymbolData>>,
}

impl Symbol {
    fn with_val(name: Atom, val: LispObj) -> Symbol {
        Symbol {
            name: name,
            data: Rc::new(RefCell::new(SymbolData {
                value: Some(val),
                function: None,
                properties: None,
            })),
        }
    }

    fn with_ext_fun<F: 'static + Func>(name: Atom, fun: F) -> Symbol {
        with_fun_obj(name, LispObj::ExtFun(fun))
    }

    fn with_fun(name: Atom, fun: LispObj) -> Symbol {
        Symbol {
            name: name,
            data: Rc::new(RefCell::new(SymbolData {
                value: None,
                function: Some(fun),
                properties: None,
            })),
        }
    }
}

/// A collection of named functions and variables
///
/// This is probably fairly close to an obarray in Emacs.
pub struct Namespace {
    syms: FnvHashMap<Atom, Symbol>,
}

impl Namespace {
    fn new() -> Namespace {
        Namespace {
            syms: FnvHashMap::default(),
        }
    }

    fn intern(&mut self, sym: Symbol) {
        syms.insert(sym.name, sym);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builtins() {
        let mut reg = AtomRegistry::with_capacity(1);

        let atm = reg.atomize("nil");
        assert!(NIL == atm);
        let _cdr = reg.atomize("cdr");
        let atm = reg.atomize("lambda");
        assert!(LAMBDA == atm);
        let atm = reg.atomize("macro");
        assert!(MACRO == atm);
    }

    #[test]
    fn same_atom() {
        let s = "magit-imenu--repolist-extract-index-name-function";
        let mut reg = AtomRegistry::with_capacity(1);

        let atm = reg.atomize(s);
        assert!(reg.atomize(s) == atm);
        assert!(reg.stringify(atm) == s);
    }

    #[test]
    fn different_atoms() {
        let s = "car";
        let t = "let";
        let mut reg = AtomRegistry::with_capacity(2);

        let u = reg.atomize(s);
        let v = reg.atomize(t);
        assert!(reg.stringify(v) == t);
        assert!(reg.stringify(u) == s);
        assert!(reg.atomize(t) == v);
        assert!(reg.atomize(s) == u);
        assert!(u != v);
        assert!(reg.stringify(u) != reg.stringify(v));
    }
}
