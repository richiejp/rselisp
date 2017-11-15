use std::borrow::Borrow;
use std::{time, thread};
use std::sync::mpsc::{Sender, Receiver, TryRecvError};
use std::sync::RwLock;
use orbclient::{self, Window, Renderer, EventOption, WindowFlag, Color};

use editor::*;

/// An OS window
///
/// Emacs has another object called a Window which exists inside
/// Frames. Windows in Emacs are tiled across the frame; so Emacs is a tiling
/// window manager.
pub trait Frame {
    fn start(&mut self);
}

#[derive(Debug)]
pub enum FrameCmd {
    Show,
    Update(Content),
    Quit,
}

pub struct OrbFrame {
    recv: Receiver<FrameCmd>,
    send: Sender<UserEvent>,
    win: Option<Window>,
    mods: EventModifiers,
}

const FG_COLOUR: Color = Color::rgb(0xbd, 0xc3, 0xce);
const BG_COLOUR: Color = Color::rgb(0x2a, 0x2f, 0x38);

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

    fn draw(win: &mut Window, x: u32, y: u32, stuff: Content) {
        let fonts: &FontCache = &*(stuff.fonts.borrow() as &RwLock<FontCache>).read().unwrap();
        let mut u = x;
        let mut v = y;

        for frag in stuff.frags {
            if let FragmentText::Indx { start: s, end: e, font: f } = frag.text {
                let chr_width = fonts.get(f as usize).width;
                for (i, chr) in stuff.text[s as usize .. e as usize].chars().enumerate() {
                    win.char(u as i32 + chr_width as i32 * i as i32, v as i32, chr, FG_COLOUR);
                }
            }

            if let Layout::FlowBreak = frag.layout {
                u = x;
                v += frag.height as u32;
            } else {
                u += frag.width as u32;
            }
        }
    }

    fn update(&mut self, doc: Content) {
        if let Some(ref mut win) = self.win {
            let (w, h) = (win.width(), win.height());

            win.rect(0, 0, w, h, BG_COLOUR);
            OrbFrame::draw(win, 4, 4, doc);
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
        self.update(Content::new());
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
