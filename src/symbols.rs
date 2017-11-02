use std::fmt;
use fnv::FnvHashMap;
use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::usize;

use super::*;

#[derive(Clone)]
pub struct Atom {
    indx: usize,
    reg: Weak<RefCell<AtomRegistry>>,
}

impl Atom {
    fn default() -> Atom {
        Atom { indx: usize::MAX, reg: Weak::new() }
    }
}

impl PartialEq for Atom {
    fn eq(&self, other: &Atom) -> bool {
        self.indx == other.indx
    }
}

impl Eq for Atom {
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let reg = self.reg.upgrade().unwrap();
        let reg = reg.borrow();
        write!(f, "{}", reg.stringify(self))
    }
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Atom({}, {})", self.indx, self)
    }
}

pub struct AtomRegistry {
    table: Vec<String>,
    rev_table: FnvHashMap<String, Atom>,
    self_ref: Weak<RefCell<AtomRegistry>>,

    pub NIL: Atom,
    pub LAMBDA: Atom,
    pub MACRO: Atom,
}

impl AtomRegistry {
    fn with_capacity(capacity: usize) -> Rc<RefCell<AtomRegistry>> {
        let cap = capacity + 3;
        let me = Rc::new(RefCell::new(AtomRegistry {
            table: Vec::with_capacity(cap),
            rev_table: FnvHashMap::with_capacity_and_hasher(cap, Default::default()),
            self_ref: Weak::new(),
            NIL: Atom::default(),
            LAMBDA: Atom::default(),
            MACRO: Atom::default(),
        }));
        {
            let mut m = me.borrow_mut();
            m.self_ref = Rc::downgrade(&me);
            m.NIL = m.atomize("nil");
            m.LAMBDA = m.atomize("lambda");
            m.MACRO = m.atomize("macro");
        }
        me
    }

    pub fn atomize(&mut self, name: &str) -> Atom {
        if let Some(atom) = self.rev_table.get(name).and_then(|atm| Some(atm.clone())) {
            atom
        } else {
            self.table.push(name.into());
            let atom = Atom {
                indx: self.table.len() - 1,
                reg: self.self_ref.clone(),
            };
            self.rev_table.insert(name.into(), atom.clone());
            atom
        }
    }

    pub fn stringify(&self, atom: &Atom) -> &str {
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
        let reg = AtomRegistry::with_capacity(1);
        let mut reg = reg.borrow_mut();

        let atm = reg.atomize("nil");
        assert_eq!(reg.NIL, atm);
        let _cdr = reg.atomize("cdr");
        let atm = reg.atomize("lambda");
        assert_eq!(reg.LAMBDA, atm);
        let atm = reg.atomize("macro");
        assert_eq!(reg.MACRO, atm);
    }

    #[test]
    fn same_atom() {
        let s = "magit-imenu--repolist-extract-index-name-function";
        let reg = AtomRegistry::with_capacity(1);
        let mut reg = reg.borrow_mut();

        let atm = reg.atomize(s);
        assert_eq!(reg.atomize(s), atm);
        assert_eq!(reg.stringify(&atm), s);
    }

    #[test]
    fn different_atoms() {
        let s = "car";
        let t = "let";
        let reg = AtomRegistry::with_capacity(2);
        let mut reg = reg.borrow_mut();

        let u = reg.atomize(s);
        let v = reg.atomize(t);
        assert_eq!(reg.stringify(&v), t);
        assert_eq!(reg.stringify(&u), s);
        assert_eq!(reg.atomize(t), v);
        assert_eq!(reg.atomize(s), u);
        assert_ne!(u, v);
        assert_ne!(reg.stringify(&u), reg.stringify(&v));
    }
}
