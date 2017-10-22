use fnv::FnvHashMap;
use std::usize;
use std::fmt;
use std::cmp;

use super::*;

macro_rules! gen_const_atoms {
    ($first:ident, $($rest:ident),*) => {
        gen_const_atoms! {
            $($rest),+ ; 0; $first = 0
        }
    };
    ($cur:ident, $($rest:ident),* ; $last_index: expr ; $($var:ident = $index:expr)+) => {
        gen_const_atoms! {
            $($rest),* ; $last_index + 1; $($var = $index)* $cur = $last_index + 1
        }
    };
    ($cur:ident; $last_index:expr ; $($var:ident = $index:expr)+) => {
        $( pub const $var: Atom = Atom { indx: $index }; )+
           pub const $cur: Atom = Atom { indx: $last_index };
    };
}

// Must be in same order as atomize_const_atoms! below
gen_const_atoms! {
    NIL, T, LAMBDA, MACRO, ANONYMOUS, QUOTE, EXIT, LOAD_PATH, KEYMAP
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct Atom {
    indx: usize,
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<a{}>", self.indx)
    }
}

pub struct AtomRegistry {
    table: Vec<String>,
    rev_table: FnvHashMap<String, Atom>,
}

impl AtomRegistry {
    pub fn with_capacity(capacity: usize) -> AtomRegistry {
        let cap = capacity + 3;
        let mut me = AtomRegistry {
            table: Vec::with_capacity(cap),
            rev_table: FnvHashMap::with_capacity_and_hasher(cap, Default::default()),
        };

        macro_rules! atomize_const_atoms {
            ( $( $name:expr ),+ ) => (
                $( me.atomize($name) );+
            )
        }

        // Must be in the same order as gen_const_atoms! above
        atomize_const_atoms!(
            "nil", "t", "lambda", "macro", "#<anonymous>", "quote", "exit", "load-path",
            "keymap"
        );
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

impl SymbolData {
    fn new(val: Option<LispObj>, fun: Option<LispObj>, props: Option<FnvHashMap<Atom, LispObj>>)
           -> Rc<RefCell<SymbolData>>
    {
        Rc::new(RefCell::new(SymbolData {
            value: val,
            function: fun,
            properties: props,
        }))
    }
}

#[derive(Clone)]
pub struct Symbol {
    pub name: Atom,
    data: Rc<RefCell<SymbolData>>,
}

impl Symbol {
    pub fn new(name: Atom) -> Symbol {
        Symbol {
            name: name,
            data: SymbolData::new(None, None, None),
        }
    }

    pub fn with_val(name: Atom, val: LispObj) -> Symbol {
        Symbol {
            name: name,
            data: SymbolData::new(Some(val), None, None)
        }
    }

    pub fn with_ext_fun<F: 'static + Func>(name: Atom, fun: F) -> Symbol {
        Symbol::with_fun(name, LispObj::extern_fun(fun))
    }

    pub fn with_fun(name: Atom, fun: LispObj) -> Symbol {
        Symbol {
            name: name,
            data: SymbolData::new(None, Some(fun), None)
        }
    }

    pub fn get_val(&self) -> Option<LispObj> {
        let data = self.data.borrow();
        if let Some(ref val) = data.value {
            Some(val.clone())
        } else {
            None
        }
    }

    pub fn get_fun(&self) -> Option<LispObj> {
        let data = self.data.borrow();
        if let Some(ref val) = data.function {
            Some(val.clone())
        } else {
            None
        }
    }

    pub fn set_fun(&self, fun: LispObj) {
        let mut data = self.data.borrow_mut();
        data.function = Some(fun);
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let data = self.data.borrow();
        write!(f, "Symbol {{ name: {:?}, val: {:?}, fun: {:?}, props: {:?} }}",
               self.name, data.value, data.function, data.properties)
    }
}

impl cmp::PartialEq for Symbol {
    fn eq(&self, other: &Symbol) -> bool {
        self.name == other.name
    }
}

/// A collection of named functions and variables
///
/// This is probably fairly close to an obarray in Emacs.
pub struct Namespace {
    syms: FnvHashMap<Atom, Symbol>,
}

impl Namespace {
    pub fn new() -> Namespace {
        Namespace {
            syms: FnvHashMap::default(),
        }
    }

    pub fn intern(&mut self, sym: Symbol) {
        self.syms.insert(sym.name, sym);
    }

    pub fn get(&self, name: Atom) -> Option<&Symbol> {
        self.syms.get(&name)
    }

    pub fn get_val(&self, name: Atom) -> Option<LispObj> {
        self.get(name).and_then( |sym| sym.get_val() )
    }

    pub fn get_fun(&self, name: Atom) -> Option<LispObj> {
        self.get(name).and_then( |sym| sym.get_fun() )
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
