use std::any::Any;
use std::borrow::Borrow;
use std::{iter, ptr, fmt};
use std::fs::File;
use std::io::Read;
use std::sync::{Arc, RwLock};
use std::str;

use rselisp::LispForm;

use editor::*;

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
            fonts: Arc::new(RwLock::new(FontCache::default())),
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

    pub fn layout(&self, cur: u16) -> (u16, Content) {
        let mut text = String::new();
        let mut itr = self.chars();
        let mut frag = Fragment::new();
        let mut frags = Vec::<Fragment>::new();
        let fonts: &FontCache = &*(self.fonts.borrow() as &RwLock<FontCache>).read().unwrap();
        let dfont: &Font = fonts.get(0);
        let mut indx = 0;
        let mut count = 0;

        macro_rules! push_frag {
            () => {
                if frag.height == 0 {
                    frag.height = dfont.height + 1;
                }
                frags.push(frag);
                frag = Fragment::new();
            }
        }

        macro_rules! set_curs {
            () => {
                frag.style = Style::Cursor;
                frag.width += dfont.width;
            }
        }

        while let Some(c) = itr.next() {
            match c {
                '\n' => {
                    if count == cur {
                        push_frag!();
                        set_curs!();
                    }
                    frag.layout = Layout::FlowBreak;
                    push_frag!();
                },
                '\t' => {
                    if count == cur {
                        push_frag!();
                        frag.style = Style::Cursor;
                    }
                    frag.width += dfont.width * 4 - frag.width % (dfont.width * 4);
                    push_frag!();
                },
                c => {
                    if count == cur {
                        push_frag!();
                        frag.text = FragmentText::Indx {
                            start: indx,
                            end: indx + 1,
                            font: 0,
                        };
                        set_curs!();
                        text.push(c);
                        push_frag!();
                    } else {
                        match frag.text {
                            FragmentText::None => frag.text = FragmentText::Indx {
                                start: indx,
                                end: indx + 1,
                                font: 0,
                            },
                            FragmentText::Indx { start: _, end: ref mut e, font: _ } => {
                                *e += 1;
                            },
                        }
                        frag.width += dfont.width;
                        text.push(c);
                    }
                    indx += 1;
                }
            }
            count += 1;
        }

        if cur >= count {
            push_frag!();
            frag.style = Style::Cursor;
            frag.width = dfont.width;
        }
        if frag.height == 0 {
            frag.height = dfont.height + 1;
        }
        frags.push(frag);

        (cur.min(count), Content {
            text: text,
            fonts: self.fonts.clone(),
            frags: frags,
        })
    }

    pub fn mode_line_layout(&self) -> Content {
        Content {
            text: "MODE LINE".to_owned(),
            fonts: self.fonts.clone(),
            frags: vec![Fragment::new()],
        }
    }
}

impl LispForm for Buffer {
    fn rust_name(&self) -> &'static str {
        "buffer::Buffer"
    }

    fn lisp_name(&self) -> &'static str {
        "buffer"
    }

    fn as_any(&mut self) -> &mut Any {
        self
    }
}

impl fmt::Debug for Buffer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Buffer {{ ... }}")
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
