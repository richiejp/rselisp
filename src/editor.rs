use std::fs::File;
use std::io::prelude::*;
use std::ptr;
use std::str;
use std::iter;
use std::thread;
use std::time;
use std::sync::mpsc::{Sender, Receiver, TryRecvError};
use std::sync::{Arc, RwLock};
use orbclient;
use orbclient::{Window, Renderer, EventOption, WindowFlag, Color};

/// Contains a textual document
///
/// This is supposed to mirror what emacs calls a buffer. Currently we are
/// just using a gap buffer, but I would prefer to use a rope/b-tree. If I
/// understand
/// https://github.com/google/xi-editor/tree/master/doc/rope_science correctly
/// then we can neatly parralise a number of operations using a rope. Of
/// course we could store any number of B-trees alongside a gap buffer for
/// containing structural/meta/index data.
///
/// For now though a gap buffer is fine and later rselisp could be implemented
/// as a plugin for Xi or the relevant parts for Xi could be vendored into
/// rselisp. Also when opening a very large file, such as a log file, we
/// should map it into memory (use mmap) so that the kernel only pages-in
/// (loads) data which is explicitly requested.
pub struct Buffer {
    gap_buf: String,
    gap_indx: usize,
    gap_len: usize,
    gap_tmpl: &'static str,
    fonts: Arc<RwLock<FontCache>>,
}

/// Iterates through the characters in the buffer's text
///
/// Skips the gap in the gap buffer.
type BufferIter<'a> = iter::Chain<iter::Take<str::Chars<'a>>, iter::Skip<str::Chars<'a>>>;

impl Buffer {

    pub fn new() -> Buffer {
        let mut s = Buffer {
            gap_buf: String::with_capacity(8196),
            gap_indx: 0,
            gap_len: 0,
            gap_tmpl: str::from_utf8(&[b' '; 1024]).unwrap(),
            fonts: Arc::new(RwLock::new(FontCache::defualt())),
        };
        s.topup_gap();
        s
    }

    /// Load a file into the buffer
    pub fn find_file(&mut self, name: &str) -> Result<(), String> {
        match File::open(name) {
            Ok(mut file) => {
                if let Err(e) = file.read_to_string(&mut self.gap_buf) {
                    Err(format!("I/O ERROR: {}", e))
                } else {
                    Ok(())
                }
            },
            Err(e) => {
                Err(format!("FILE ERROR: {}", e))
            },
        }
    }

    unsafe fn mov_gap(&mut self, indx: usize) {
        let sptr = self.gap_buf.as_mut_vec().as_ptr();
        let msptr = self.gap_buf.as_mut_vec().as_mut_ptr();
        let new = indx as isize;
        let cur = self.gap_indx as isize;
        let gap = self.gap_len as isize;

        let (from, to, len) = if new > cur {
            (cur + gap, cur, new - cur)
        } else {
            (new, new + gap, cur - new)
        };

        ptr::copy(sptr.offset(from), msptr.offset(to), len as usize);
        self.gap_indx = indx;
    }

    fn topup_gap(&mut self) {
        self.gap_buf.insert_str(self.gap_indx + self.gap_len,
                                &self.gap_tmpl[self.gap_len..]);
        self.gap_len = self.gap_tmpl.len();
    }

    pub fn len(&self) -> usize {
        self.gap_buf.len() - self.gap_len
    }

    pub fn insert(&mut self, indx: usize, text: &str) {
        debug_assert!(indx <= self.len());
        if indx != self.gap_indx {
            unsafe { self.mov_gap(indx); }
        }

        // If it is a big string the user is probably pasting text and we
        // don't care so much about responsiveness. So save the gap for
        // keystrokes and just do a normal insert.
        if text.len() > self.gap_tmpl.len() / 4 {
            self.gap_buf.insert_str(self.gap_indx, text);
            self.gap_indx += text.len();
            return;
        }

        if text.len() >= self.gap_len {
            self.topup_gap();
        }

        unsafe {
            let dst = self.gap_buf.as_mut_vec().as_mut_ptr();
            ptr::copy(text.as_ptr(), dst.offset(self.gap_indx as isize), text.len());
        }

        self.gap_indx += text.len();
        self.gap_len -= text.len();
    }

    pub fn chars(&self) -> BufferIter {
        self.gap_buf.chars()
            .take(self.gap_indx)
            .chain(self.gap_buf.chars().skip(self.gap_indx + self.gap_len))
    }

    pub fn layout(&self) -> Content {
        let frags = Vec::new();
        let text = String::new();
        let itr = self.chars();
        let mut frag = Fragment::new();
        let mut frags = Vec::<Fragment>::new();
        let dfont = &self.fonts.borrow().read().unwrap().array[0];

        let push_frag = || {
            frags.push(frag.clone());
            frag = Fragment::new();
            frag.height = dfont.height;
        };

        while let Some(c) = itr.next() {
            match c {
                '\n' => {
                    frag.layout = Layout::FlowBreak;
                    push_frag();
                },
                '\t' => {
                    frag.width += frag.width % (dfont.width * 4);
                    push_frag();
                },
                c => {
                    match frag.text {
                        FragmentText::None => frag.text = FragmentText::Indx {
                            start: indx,
                            end: indx,
                            font: 0,
                        },
                        FragmentText::Indx { start: _, end: mut ref e, font: _ } => {
                            e++;
                        },
                    }
                    frag.width += dfont.width;
                    text.push(c);
                }
            }
        }

        Content {
            text: text,
            fonts: self.fonts.clone(),
            frags: frags,
        }
    }

    pub fn mode_line(&self) -> String {
        " U:**-  *scratch*\tAll (0,0)\t(Fake mode line)".to_owned()
    }
}

struct Font {
    index: u8,
    name: String,
    width: u16,
    height: u16,
}

impl Font {
    fn default() -> Font {
        index: 0,
        name: "unifont".to_owned(),
        width: 8,
        height: 16,
    }
}

struct FontCache {
    array: Vec<Font>,
    names: FnvHashMap<String, u8>,
}

impl FontCache {
    fn default() -> FontCache {
        FontCache {
            array: vec![Font::default()],
            names: [("unifont", 0)].iter().cloned().collect(),
        }
    }
}

#[derive(Clone)]
enum FragmentText {
    None,
    Indx {
        start: u16,
        end: u16,
        font: u8,
    },
}

#[derive(Clone)]
enum Layout {
    Flow,
    FlowBreak,
}

#[derive(Clone)]
struct Fragment {
    text: FragmentText,
    width: u16,
    height: u16,
    layout: Layout,
}

impl Fragment {
    fn new() -> Fragment {
        Fragment {
            text: FragmentText::None,
            width: 0,
            height: 0,
            layout: Layout::Flow,
        }
    }
}

struct Content {
    text: String,
    fonts: Arc<RwLock<FontCache>>,
    frags: Vec<Fragment>,
}

#[derive(Debug)]
pub enum FrameCmd {
    Show,
    Update(Content),
    Quit,
}

#[derive(Debug)]
pub enum UserEvent {
    Key(Event),
    Quit,
}

impl UserEvent {
    fn new_keyevent(basic: BasicEvent, mods: EventModifiers) -> UserEvent {
        UserEvent::Key(Event::new(basic, mods))
    }
}

/// An OS window
///
/// Emacs has another object called a Window which exists inside
/// Frames. Windows in Emacs are tiled across the frame; so Emacs is a tiling
/// window manager.
pub trait Frame {
    fn start(&mut self);
}

/// What keys were held down during an event
#[derive(Clone, Debug)]
pub struct EventModifiers {
    control: bool,
    shift: bool,
    /// Also super, win or mac key
    hyper: bool,
    /// Also the meta key
    alt: bool,
}

impl EventModifiers {
    fn new() -> EventModifiers {
        EventModifiers {
            control: false,
            shift: false,
            hyper: false,
            alt: false,
        }
    }
}

/// Emacs calls the key/button pressed the basic part of an event
#[derive(Debug)]
pub enum BasicEvent {
    Backspace,
    Del,
    Char(char),
}

/// An Emacs event
///
/// Events in Emacs appear to be limited to key and button presses by the
/// user.
#[derive(Debug)]
pub struct Event {
    pub basic: BasicEvent,
    pub modifiers: EventModifiers,
}

impl Event {
    fn new(basic: BasicEvent, mods: EventModifiers) -> Event {
        Event {
            basic: basic,
            modifiers: mods,
        }
    }
}

pub struct OrbFrame {
    recv: Receiver<FrameCmd>,
    send: Sender<UserEvent>,
    win: Option<Window>,
    mods: EventModifiers,
}

/// The result of trying to communicate with some other component.
enum ComResult {
    /// Number of messages received
    Recvd(u32),
    /// We should quit for some reason
    Quit,
}

impl OrbFrame {
    pub fn new(send: Sender<UserEvent>, recv: Receiver<FrameCmd>) -> OrbFrame {
        OrbFrame {
            recv: recv,
            send: send,
            win: None,
            mods: EventModifiers::new(),
        }
    }

    fn draw_str(win: &mut Window, x: i32, y: i32, txt: &str, colour: Color) {
        let mut line = 0;
        let mut col = 0;

        for chr in txt.chars() {
            match chr {
                '\n' => {
                    line += 1;
                    col = 0;
                },
                '\t' => {
                    col += 4;
                },
                _ => {
                    win.char(x + 8 * col, y + 16 * line, chr, colour);
                    col += 1;
                },
            }
        }
    }

    fn draw_text_box(win: &mut Window, x: u32, y: u32, w: u32, h: u32, txt: &str,
                     fg: Color, bg: Color) {
        win.rect(x as i32, y as i32, w, h, bg);
        OrbFrame::draw_str(win, x as i32, y as i32, txt, fg);
    }

    fn update(&mut self, doc: String) {
        if let Some(ref mut win) = self.win {
            let (w, h) = (win.width(), win.height());

            OrbFrame::draw_text_box(win, 0, 0, w, h - 32, &doc,
                                    Color::rgb(0xbd, 0xc3, 0xce),
                                    Color::rgb(0x2a, 0x2f, 0x38));
            OrbFrame::draw_text_box(win, 0, h - 32, w, 16, "MODE LINE",
                                    Color::rgb(0xbd, 0xc3, 0xce),
                                    Color::rgb(0x24, 0x2a, 0x34));
            OrbFrame::draw_text_box(win, 0, h - 16, w, 16, "Echo echo ...",
                                    Color::rgb(0xbd, 0xc3, 0xce),
                                    Color::rgb(0x2a, 0x2f, 0x38));
            win.sync();
        }
    }

    fn recv(&mut self) -> ComResult {
        let mut cnt = 0;

        loop {
            let res = self.recv.try_recv();
            match res {
                Ok(cmd) => {
                    cnt += 1;
                    match cmd {
                        FrameCmd::Show => self.show(),
                        FrameCmd::Quit => return ComResult::Quit,
                        FrameCmd::Update(doc) => self.update(doc),
                    }
                },
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => {
                    println!("OrbFrame is orphaned!");
                    return ComResult::Quit;
                },
            };
        }

        ComResult::Recvd(cnt)
    }

    fn show(&mut self) {
        let (width, height) = orbclient::get_display_size().unwrap();
        self.win = Some(Window::new_flags(width as i32 / 4, height as i32 / 4,
                                          700, 500,
                                          &"rselisp",
                                          &[WindowFlag::Async]).unwrap());
        self.update("".to_owned());
    }

    /// React to user input events
    fn react(&mut self) -> ComResult {
        let mut cnt = 0;

        macro_rules! send {
            ($thing:expr) => {{
                if let Err(_) = self.send.send($thing) {
                    return ComResult::Quit;
                }
            }}
        }

        macro_rules! send_key {
            ($key:ident) => {
                send!(UserEvent::Key(Event::new(BasicEvent::$key, self.mods.clone())))
            }
        }

        if let Some(ref mut win) = self.win {
            for event in win.events() {
                cnt += 1;
                match event.to_option() {
                    EventOption::Quit(_quit_event) => send!(UserEvent::Quit),
                    EventOption::Key(key) => if key.pressed {
                        match key.scancode {
                            56 => self.mods.alt = true,
                            29 => self.mods.control = true,
                            42 | 54 => self.mods.shift = true,
                            14 => send_key!(Backspace),
                            83 => send_key!(Del),
                            sc => if key.character == '\0' {
                                println!("Unhandled key; scancode: {}", sc);
                            } else {
                                send!(UserEvent::new_keyevent(BasicEvent::Char(key.character),
                                                              self.mods.clone()));
                            },
                        }
                    } else {
                        match key.scancode {
                            56 => self.mods.alt = false,
                            29 => self.mods.control = false,
                            42 | 54 => self.mods.shift = false,
                            _ => (),
                        }
                    },
                    EventOption::Focus(f) if !f.focused => self.mods = EventModifiers::new(),
                    event_option => println!("Unhandled event: {:?}", event_option)
                }
            }
        }

        ComResult::Recvd(cnt)
    }
}

impl Frame for OrbFrame {
    /// Run the main loop for the UI
    ///
    /// On Linux atleast; this appears to use up a lot of CPU time in the SDL
    /// library polling for user input. Ideally, during quite periods, this
    /// thread should sleep until woken by a signal from the main thread or
    /// the display manager. This probably requires a change to Orbital, so
    /// for now it just sleeps for a set period of time if no messages/events
    /// were received.
    fn start(&mut self) {
        let time = time::Duration::from_millis(30);

        loop {
            let mut cnt = 0;

            match self.recv() {
                ComResult::Recvd(recvd) => cnt += recvd,
                ComResult::Quit => break,
            }
            match self.react() {
                ComResult::Recvd(evnts) => cnt += evnts,
                ComResult::Quit => break,
            }

            if cnt < 1 {
                thread::sleep(time);
            } else {
                thread::yield_now();
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
