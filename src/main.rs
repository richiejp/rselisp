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

use std::io::stdin;
use std::fs::File;
use std::io::prelude::*;

extern crate rselisp;
use rselisp::{Lsp, Inner};

mod editor;
use editor::Buffer;

fn repl() {
    let mut lsp = Lsp::new();
    
    println!("'(rselisp repl v0.0 (C) 2017 Richard Palethorpe)");

    loop {
        let mut line = String::new();
        match stdin().read_line(&mut line) {
            Ok(_) => {
                match lsp.read(&line) {
                    Ok(sexp) => match lsp.eval(&sexp) {
                        Ok(Inner::Sym(ref s)) if s == "exit" => break,
                        Ok(resexp) => println!("-> {}", resexp),
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
    let mut args = std::env::args();
    if let (Some(_exe_name), Some(file)) = (args.next(), args.next()) {
        exec_file(&file);
    } else {
        repl();
    }
}
