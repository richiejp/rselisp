use std::fmt;
use std::sync::{Arc, RwLock};
use fnv::FnvHashMap;

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
pub struct Fragment {
    pub text: FragmentText,
    pub width: u16,
    pub height: u16,
    pub layout: Layout,
}

impl Fragment {
    pub fn new() -> Fragment {
        Fragment {
            text: FragmentText::None,
            width: 0,
            height: 0,
            layout: Layout::Flow,
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

/// Emacs calls the key/button pressed the basic part of an event
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BasicEvent {
    Backspace,
    Del,
    Char(char),
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

/// The result of trying to communicate with some other component.
pub enum ComResult {
    /// Number of messages received
    Recvd(u32),
    /// We should quit for some reason
    Quit,
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
