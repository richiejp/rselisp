use std::fmt;
use std::sync::{Arc, RwLock};
use std::sync::mpsc::channel;
use std::thread;
use fnv::FnvHashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::any::Any;

use rselisp::{Lsp, LispObj, Sexp, LispForm, External};
use rselisp::symbols::{self, Symbol};
use rselisp::lambda::Func;

use frame::{Frame, OrbFrame, FrameCmd};
use buffer::Buffer;
use keymap::{Keymap, KeymapBuiltin, DefineKeyBuiltin};

pub struct Font {
    //index: u8,
    pub name: String,
    pub width: u16,
    pub height: u16,
}

impl Font {
    pub fn default() -> Font {
        Font {
            //index: 0,
            name: "unifont".to_owned(),
            width: 8,
            height: 16,
        }
    }
}

pub struct FontCache {
    array: Vec<Font>,
    //names: FnvHashMap<String, u8>,
}

impl FontCache {
    pub fn default() -> FontCache {
        let mut names = FnvHashMap::<String, u8>::default();

        names.insert("unifont".to_owned(), 0);

        FontCache {
            array: vec![Font::default()],
            //names: names,
        }
    }

    pub fn get(&self, i: usize) -> &Font {
        &self.array[i]
    }
}

#[derive(Clone, Debug)]
pub enum FragmentText {
    None,
    Indx {
        start: u16,
        end: u16,
        font: u8,
    },
}

#[derive(Clone, Debug)]
pub enum Layout {
    Flow,
    FlowBreak,
}

#[derive(Clone, Debug)]
pub enum Style {
    Default,
    Cursor,
}

#[derive(Clone, Debug)]
pub struct Fragment {
    pub text: FragmentText,
    pub width: u16,
    pub height: u16,
    pub layout: Layout,
    pub style: Style,
}

impl Fragment {
    pub fn new() -> Fragment {
        Fragment {
            text: FragmentText::None,
            width: 0,
            height: 0,
            layout: Layout::Flow,
            style: Style::Default,
        }
    }
}

pub struct Content {
    pub text: String,
    pub fonts: Arc<RwLock<FontCache>>,
    pub frags: Vec<Fragment>,
}

impl Content {
    pub fn new() -> Content {
        Content {
            text: String::new(),
            fonts: Arc::<RwLock<FontCache>>::new(RwLock::new(FontCache::default())),
            frags: Vec::<Fragment>::new(),
        }
    }
}

impl fmt::Debug for Content {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Content {{ text: {:?}, frags: {:?} }}", self.text, self.frags)
    }
}

#[derive(Debug)]
pub enum UserEvent {
    Key(Event),
    Quit,
}

impl UserEvent {
    pub fn new_keyevent(basic: BasicEvent, mods: EventModifiers) -> UserEvent {
        UserEvent::Key(Event::new(basic, mods))
    }
}

/// What keys were held down during an event
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct EventModifiers {
    pub control: bool,
    pub shift: bool,
    /// Also super, win or mac key
    pub hyper: bool,
    /// Also the meta key
    pub alt: bool,
}

impl EventModifiers {
    pub fn new() -> EventModifiers {
        EventModifiers {
            control: false,
            shift: false,
            hyper: false,
            alt: false,
        }
    }
}

impl LispForm for EventModifiers {
    fn rust_name(&self) -> &'static str {
        "Editor::EventModifiers"
    }

    fn lisp_name(&self) -> &'static str {
        ""
    }

    fn to_lisp(&self) -> Result<LispObj, String> {
        let mut mods = Sexp::new('[');

        macro_rules! c {
            ($field:ident) => {
                if self.$field { mods.push(LispObj::Str("$field".to_owned())) }
            }
        }

        c!(control);
        c!(shift);
        c!(hyper);
        c!(alt);

        Ok(LispObj::Sxp(mods))
    }

    fn as_any(&mut self) -> &mut Any {
        self
    }
}

/// Emacs calls the key/button pressed the basic part of an event
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BasicEvent {
    Backspace,
    Del,
    Char(char),
}

impl LispForm for BasicEvent {
    fn rust_name(&self) -> &'static str {
        "Editor::BasicEvent"
    }

    fn lisp_name(&self) -> &'static str {
        ""
    }

    fn to_lisp(&self) -> Result<LispObj, String> {
        Ok(match self {
            &BasicEvent::Backspace => LispObj::Sxp(Sexp::vec_from(&[LispObj::Str("backspace".to_owned())])),
            &BasicEvent::Del => LispObj::Sxp(Sexp::vec_from(&[LispObj::Str("delete".to_owned())])),
            &BasicEvent::Char(c) => LispObj::Str(c.to_string().to_owned()),
        })
    }

    fn as_any(&mut self) -> &mut Any {
        self
    }
}

/// An Emacs event
///
/// Events in Emacs appear to be limited to key and button presses by the
/// user.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Event {
    pub basic: BasicEvent,
    pub modifiers: EventModifiers,
}

impl Event {
    pub fn new(basic: BasicEvent, mods: EventModifiers) -> Event {
        Event {
            basic: basic,
            modifiers: mods,
        }
    }
}

impl LispForm for Event {
    fn rust_name(&self) -> &'static str {
        "Editor::Event"
    }

    fn lisp_name(&self) -> &'static str {
        "event"
    }

    fn to_lisp(&self) -> Result<LispObj, String> {
        Ok(LispObj::Sxp(Sexp::from(&[
            LispObj::Str("basic".to_owned()), self.basic.to_lisp()?,
            LispObj::Str("modifiers".to_owned()), self.modifiers.to_lisp()?,
        ])))
    }

    fn as_any(&mut self) -> &mut Any {
        self
    }
}

/// The result of trying to communicate with some other component.
pub enum ComResult {
    /// Number of messages received
    Recvd(u32),
    /// We should quit for some reason
    Quit,
}

/// The blinky thing which text comes out of
#[derive(Clone, Copy, Debug)]
pub struct Cursor {
    //mark: usize,
    //scroll: usize,
    index: usize,
}

impl Cursor {
    fn new() -> Cursor {
        Cursor {
            //scroll: 0,
            index: 0,
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    fn mov(&mut self, n: isize) {
        if n > 0 {
            self.index += n as usize;
        } else {
            self.index -= n.abs() as usize;
        }
    }
}

pub fn start() {
    let (frm_cmd_send, frm_cmd_recv) = channel::<FrameCmd>();
    let (frm_evt_send, frm_evt_recv) = channel::<UserEvent>();
    let _thrd = thread::spawn( move || {
        let mut frame = OrbFrame::new(frm_evt_send, frm_cmd_recv);
        frame.start();
    });
    let bufcell = Rc::new(RefCell::new(Buffer::new()));
    //let echo_bufcell = Rc::new(RefCell::new(Buffer::new()));
    //let mini_bufcell = Rc::new(RefCell::new(Buffer::new()));
    let mut cbuf = String::with_capacity(4);
    let mut cursor = Cursor::new();
    let global_keymapcell = Rc::new(RefCell::new(Keymap::new()));
    let mut lsp = Lsp::new();

    reg_funcs!(lsp; KeymapBuiltin, DefineKeyBuiltin);
    lsp.set_global("global-map",
                   LispObj::Ext(Rc::clone(&global_keymapcell) as External));
    if let Err(e) = lsp.load("editor") {
        println!("LISP ERROR: {}", e);
        return;
    }

    frm_cmd_send.send(FrameCmd::Show).unwrap();
    {
        let buf = &*bufcell.borrow();
        frm_cmd_send.send(FrameCmd::Update(buf.layout(cursor))).unwrap();
    }

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
                    global_keymap.lookup_key(&kevt).and_then( |act| {
                        Some(act.clone())
                    })
                };
                if let Some(action) = lookup {
                    match lsp.eval_inner(&action) {
                        Err(e) => println!("LISP ERROR: {}", e),
                        Ok(LispObj::Atm(symbols::EXIT)) => break,
                        s => println!("LISP SAYS: {:?}", s),
                    }
                } else {
                    let buf = &mut *bufcell.borrow_mut();

                    match kevt {
                        Event { basic: BasicEvent::Char(c), modifiers: _ } => {
                            cbuf.push(c);
                            buf.insert(&cursor, &cbuf);
                            cbuf.pop();
                            cursor.mov(1);
                            frm_cmd_send.send(FrameCmd::Update(buf.layout(cursor))).unwrap();
                        },
                        bevt => println!("Unhandled {:?}", bevt),
                    }
                }
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_file() {
        let fname = "lisp/demo.el";
        let mut ebuf = Buffer::new();

        assert_eq!(Ok(()), ebuf.find_file(fname));
        assert!(ebuf.gap_buf.len() > 0);
    }

    #[test]
    fn insert_small() {
        let mut ebuf = Buffer::new();

        ebuf.insert(0, "Blh");
        assert_eq!(&ebuf.gap_buf[..3], "Blh");
        assert_eq!(ebuf.gap_indx, 3);

        ebuf.insert(2, "aaaa");
        let res: String = ebuf.chars().collect();
        assert_eq!(&res, "Blaaaah");
    }
}
