use fnv::FnvHashMap;
use std::usize;

use super::*;

pub static NIL: Atom = Atom { indx: 0 };
pub static T: Atom = Atom { indx: 1 };
pub static LAMBDA: Atom = Atom { indx: 2 };
pub static MACRO: Atom = Atom { indx: 3 };

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
        let cap = capacity + 3;
        let mut me = AtomRegistry {
            table: Vec::with_capacity(cap),
            rev_table: FnvHashMap::with_capacity_and_hasher(cap, Default::default()),
        };
        me.atomize("nil");
        me.atomize("t");
        me.atomize("lambda");
        me.atomize("macro");
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

pub struct Symbol {
    pub name: Atom,
    pub value: Option<LispObj>,
    pub function: Option<LispObj>,
    pub properties: FnvHashMap<Atom, LispObj>,
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
