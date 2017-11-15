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

#![feature(const_fn)]

use std::io::stdin;
use std::fs::File;
use std::io::prelude::*;

extern crate fnv;
extern crate orbclient;

#[macro_use]
extern crate rselisp;
use rselisp::{Lsp, LispObj};
use rselisp::symbols;

mod editor;
mod buffer;
mod frame;
mod keymap;

enum Mode {
    Repl,
    Editor,
    ExecFile,
}

fn repl() {
    let mut lsp = Lsp::new();
    let mut obuf = String::new();

    println!("'(rselisp repl v0.0 (C) 2017 Richard Palethorpe)");

    loop {
        let mut line = String::new();
        match stdin().read_line(&mut line) {
            Ok(_) => {
                match lsp.read(&line) {
                    Ok(sexp) => match lsp.eval(&sexp) {
                        Ok(LispObj::Atm(symbols::EXIT)) => break,
                        Ok(obj) => {
                            let _res = lsp.print(&mut obuf, &obj);
                            println!("-> {}", obuf);
                            obuf.clear();
                        },
                        Err(e) => println!("EVAL ERROR: {}", e),
                    },
                    Err(e) => println!("READ ERROR: {}", e),
                }
            },
            Err(e) => { println!("I/O ERROR: {}", e); break; },
        }
    }

    println!("'(Good bye!)");
}

fn exec_file(name: &str) {
    let mut src = String::new();
    let mut lsp = Lsp::new();

    match File::open(name) {
        Ok(mut file) => {
            if let Err(e) = file.read_to_string(&mut src) {
                println!("I/O ERROR: {}", e);
                return;
            }
        },
        Err(e) => {
            println!("FILE ERROR: {}", e);
            return;
        },
    }

    match lsp.read(&src) {
        Ok(sexp) => if let Err(e) = lsp.eval(&sexp) {
            println!("EVAL ERROR: {}", e)
        },
        Err(e) => println!("READ ERROR: {}", e),
    };
}

fn main() {
    let mut mode = Mode::Repl;
    let mut start_file: Option<String> = None;
    let mut args = std::env::args().skip(1);

    while let Some(arg) = args.next() {
        match arg.as_ref() {
            "--editor" => mode = Mode::Editor,
            "--exec" => mode = Mode::ExecFile,
            file => {
                start_file = Some(file.to_owned());
                if let Mode::Repl = mode {
                    mode = Mode::ExecFile;
                }
            },
        }
    }

    match mode {
        Mode::Editor => editor::start(),
        Mode::ExecFile => {
            if let Some(ref file) = start_file {
                exec_file(file);
            } else {
                println!("Argument --exec requires a file path");
            }
        },
        Mode::Repl => repl(),
    }
}
