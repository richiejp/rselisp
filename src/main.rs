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

use std::any::Any;
use std::io::stdin;
use std::fs::File;
use std::io::prelude::*;
use std::sync::mpsc::channel;
use std::thread;
use std::rc::Rc;
use std::cell::RefCell;

extern crate fnv;
extern crate orbclient;

#[macro_use]
extern crate rselisp;
use rselisp::{Lsp, Inner, Sexp, LispForm};

mod editor;
use editor::{Buffer, Frame, OrbFrame, FrameCmd, Event, UserEvent, BasicEvent, EventModifiers};

mod keymap;
use keymap::{Keymap, KeymapBuiltin, DefineKeyBuiltin};

enum Mode {
    Repl,
    Editor,
    ExecFile,
}

impl LispForm for BasicEvent {
    fn rust_name(&self) -> &'static str {
        "Editor::BasicEvent"
    }

    fn lisp_name(&self) -> &'static str {
        ""
    }

    fn to_lisp(&self) -> Result<Inner, String> {
        Ok(match self {
            &BasicEvent::Backspace => Inner::Sxp(Sexp::vec_from(&[Inner::Sym("backspace".to_owned())])),
            &BasicEvent::Del => Inner::Sxp(Sexp::vec_from(&[Inner::Sym("delete".to_owned())])),
            &BasicEvent::Char(c) => Inner::Str(c.to_string().to_owned()),
        })
    }

    fn as_any(&mut self) -> &mut Any {
        self
    }
}

impl LispForm for EventModifiers {
    fn rust_name(&self) -> &'static str {
        "Editor::EventModifiers"
    }

    fn lisp_name(&self) -> &'static str {
        ""
    }

    fn to_lisp(&self) -> Result<Inner, String> {
        let mut mods = Sexp::new('[');

        macro_rules! c {
            ($field:ident) => {
                if self.$field { mods.push(Inner::Sym("$field".to_owned())) }
            }
        }

        c!(control);
        c!(shift);
        c!(hyper);
        c!(alt);

        Ok(Inner::Sxp(mods))
    }

    fn as_any(&mut self) -> &mut Any {
        self
    }
}

impl LispForm for Event {
    fn rust_name(&self) -> &'static str {
        "Editor::Event"
    }

    fn lisp_name(&self) -> &'static str {
        "event"
    }

    fn to_lisp(&self) -> Result<Inner, String> {
        Ok(Inner::Sxp(Sexp::from(&[
            Inner::Sym(":basic".to_owned()), self.basic.to_lisp()?,
            Inner::Sym(":modifiers".to_owned()), self.modifiers.to_lisp()?,
        ])))
    }

    fn as_any(&mut self) -> &mut Any {
        self
    }
}

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

fn editor() {
    let (frm_cmd_send, frm_cmd_recv) = channel::<FrameCmd>();
    let (frm_evt_send, frm_evt_recv) = channel::<UserEvent>();
    let _thrd = thread::spawn( move || {
        let mut frame = OrbFrame::new(frm_evt_send, frm_cmd_recv);
        frame.start();
    });
    let bufcell = Rc::new(RefCell::new(Buffer::new()));
    let echo_bufcell = Rc::new(RefCell::new(Buffer::new()));
    let mini_bufcell = Rc::new(RefCell::new(Buffer::new()));
    let mut cbuf = String::with_capacity(4);
    let mut cursor = 0;
    let global_keymapcell = Rc::new(RefCell::new(Keymap::new()));
    let mut lsp = Lsp::new();

    lsp.globals.reg_fn(KeymapBuiltin { });
    lsp.globals.reg_fn(DefineKeyBuiltin { });

    lsp.globals.reg_var("global-map", &Inner::Ext(Rc::clone(&global_keymapcell) as Rc<RefCell<LispForm>>));

    frm_cmd_send.send(FrameCmd::Show).unwrap();

    while let Ok(evt) = frm_evt_recv.recv() {
        println!("RECEIVED EVENT: {:?}", evt);
        match evt {
            UserEvent::Quit => {
                frm_cmd_send.send(FrameCmd::Quit).unwrap();
                break;
            },
            UserEvent::Key(kevt) => {
                let lookup = {
                    let global_keymap = &*global_keymapcell.borrow();
                    if let Some(act) = global_keymap.lookup_key(&kevt) {
                        Some(act.clone())
                    } else {
                        None
                    }
                };
                if let Some(action) = lookup {
                    match lsp.eval_inner(&action) {
                        Err(e) => println!("LISP ERROR: {}", e),
                        Ok(Inner::Sym(ref s)) if s == "exit" => break,
                        s => println!("LISP SAYS: {:?}", s),
                    }
                } else {
                    let buf = &mut *bufcell.borrow_mut();

                    match kevt {
                        Event { basic: BasicEvent::Char(c), modifiers: _ } => {
                            cbuf.push(c);
                            buf.insert(cursor, &cbuf);
                            cbuf.pop();
                            frm_cmd_send.send(FrameCmd::Update(buf.layout())).unwrap();
                            cursor += 1;
                        },
                        bevt => println!("Unhandled {:?}", bevt),
                    }
                }
            }
        }
    }
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
        Mode::Editor => editor(),
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
