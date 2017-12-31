#![feature(proc_macro)]

extern crate rselisp;
extern crate rselisp_macros;

use std::slice::Iter;

use rselisp::{Lsp, LispObj};
use rselisp_macros::lisp_fn;

#[lisp_fn]
fn foobar(_lsp: &mut Lsp, _args: &mut Iter<LispObj>) -> Result<LispObj, String> {
    Ok(LispObj::Int(0xDEADC0DEi32))
}
