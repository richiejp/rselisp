use std::any::Any;
use std::borrow::Borrow;
use std::{iter, fmt};
use unicode_segmentation::UnicodeSegmentation;
// use std::fs::File;
// use std::io::Read;
use std::sync::{Arc, RwLock};
use std::str;

use rselisp::LispForm;

use crate::editor::*;

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
    gap_buf: Vec<u8>,
    gap_indx: usize,
    gap_len: usize,
    gap_tmpl: &'static str,
    fonts: Arc<RwLock<FontCache>>,
}

/// Iterates through the characters in the buffer's text
///
/// Skips the gap in the gap buffer.
type BufferIter<'a> = iter::Chain<str::Chars<'a>, str::Chars<'a>>;

impl Buffer {

    pub fn new() -> Buffer {
        let mut s = Buffer {
            gap_buf: Vec::with_capacity(8196),
            gap_indx: 0,
            gap_len: 0,
            gap_tmpl: str::from_utf8(&[b' '; 1024]).unwrap(),
            fonts: Arc::new(RwLock::new(FontCache::default())),
        };
        s.topup_gap();
        s
    }

    /// Load a file into the buffer
    // pub fn find_file(&mut self, name: &str) -> Result<(), String> {
    //     match File::open(name) {
    //         Ok(mut file) => {
    //             if let Err(e) = file.read_to_string(&mut self.gap_buf) {
    //                 Err(format!("I/O ERROR: {}", e))
    //             } else {
    //                 Ok(())
    //             }
    //         },
    //         Err(e) => {
    //             Err(format!("FILE ERROR: {}", e))
    //         },
    //     }
    // }

    fn pre(&self) -> &str {
        str::from_utf8(&self.gap_buf[..self.gap_indx])
            .expect("text before the gap must be valid UTF-8")
    }

    fn post(&self) -> &str {
        str::from_utf8(&self.gap_buf[self.gap_indx + self.gap_len..])
            .expect("text after the gap must be valid UTF-8")
    }

    pub fn is_char_boundary(&self, indx: usize) -> bool {
        if indx <= self.gap_indx {
            self.pre().is_char_boundary(indx)
        } else {
            self.post().is_char_boundary(indx - self.gap_indx)
        }
    }

    fn mov_gap(&mut self, indx: usize) {
        assert!(self.is_char_boundary(indx), "invalid UTF-8 byte index");
        if indx > self.gap_indx {
            self.gap_buf.copy_within(
                self.gap_indx + self.gap_len..indx + self.gap_len,
                self.gap_indx,
            );
        } else {
            self.gap_buf.copy_within(indx..self.gap_indx, indx + self.gap_len);
        }
        self.gap_indx = indx;
    }

    fn topup_gap(&mut self) {
        let end = self.gap_indx + self.gap_len;
        self.gap_buf.splice(end..end, self.gap_tmpl.as_bytes()[self.gap_len..].iter().copied());
        self.gap_len = self.gap_tmpl.len();
    }

    pub fn len(&self) -> usize {
        self.gap_buf.len() - self.gap_len
    }

    pub fn insert(&mut self, indx: usize, text: &str) {
        assert!(self.is_char_boundary(indx), "invalid UTF-8 byte index");
        if indx != self.gap_indx {
            self.mov_gap(indx);
        }

        // If it is a big string the user is probably pasting text and we
        // don't care so much about responsiveness. So save the gap for
        // keystrokes and just do a normal insert.
        if text.len() > self.gap_tmpl.len() / 4 {
            self.gap_buf.splice(self.gap_indx..self.gap_indx, text.bytes());
            self.gap_indx += text.len();
            return;
        }

        if text.len() >= self.gap_len {
            self.topup_gap();
        }

        self.gap_buf[self.gap_indx..self.gap_indx + text.len()]
            .copy_from_slice(text.as_bytes());

        self.gap_indx += text.len();
        self.gap_len -= text.len();
    }

    pub fn chars(&self) -> BufferIter<'_> {
        self.pre().chars().chain(self.post().chars())
    }

    fn text(&self) -> String {
        // Segment the whole document: a grapheme may span the gap.
        let mut text = String::with_capacity(self.len());
        text.push_str(self.pre());
        text.push_str(self.post());
        text
    }

    /// Map a grapheme position (including the end) to a logical byte offset.
    pub fn grapheme_to_byte(&self, index: usize) -> Option<usize> {
        let text = self.text();
        text.grapheme_indices(true).map(|(offset, _)| offset)
            .chain(std::iter::once(text.len())).nth(index)
    }

    /// Position at or immediately after a byte offset, used after insertion.
    pub fn grapheme_at_or_after(&self, byte: usize) -> usize {
        let text = self.text();
        assert!(text.is_char_boundary(byte));
        text.grapheme_indices(true).take_while(|(offset, _)| *offset < byte).count()
    }

    pub fn layout(&self, cur: usize) -> (usize, Content) {
        let mut text = String::new();
        let source = self.text();
        let mut itr = source.graphemes(true);
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
                "\n" | "\r\n" => {
                    if count == cur {
                        push_frag!();
                        set_curs!();
                    }
                    frag.layout = Layout::FlowBreak;
                    push_frag!();
                },
                "\t" => {
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
                            end: indx + c.len(),
                            font: 0,
                        };
                        set_curs!();
                        text.push_str(c);
                        push_frag!();
                    } else {
                        match frag.text {
                            FragmentText::None => frag.text = FragmentText::Indx {
                                start: indx,
                                end: indx + c.len(),
                                font: 0,
                            },
                            FragmentText::Indx { start: _, end: ref mut e, font: _ } => {
                                *e += c.len();
                            },
                        }
                        frag.width += dfont.width;
                        text.push_str(c);
                    }
                    indx += c.len();
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

    // pub fn mode_line_layout(&self) -> Content {
    //     Content {
    //         text: "MODE LINE".to_owned(),
    //         fonts: self.fonts.clone(),
    //         frags: vec![Fragment::new()],
    //     }
    // }
}

impl LispForm for Buffer {
    fn rust_name(&self) -> &'static str {
        "buffer::Buffer"
    }

    fn lisp_name(&self) -> &'static str {
        "buffer"
    }

    fn as_any(&mut self) -> &mut dyn Any {
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

    // #[test]
    // fn find_file() {
    //     let fname = "lisp/demo.el";
    //     let mut ebuf = Buffer::new();
    //
    //     assert_eq!(Ok(()), ebuf.find_file(fname));
    //     assert!(ebuf.gap_buf.len() > 0);
    // }

    #[test]
    fn unicode_gap_moves_and_refills() {
        let mut buffer = Buffer::new();
        let mut expected = "é中🙂".repeat(100);
        buffer.insert(0, &expected);
        for i in 0..600 {
            let byte = if i % 2 == 0 { "é".len() } else { expected.len() };
            buffer.insert(byte, "é");
            expected.insert_str(byte, "é");
            assert_eq!(buffer.chars().collect::<String>(), expected);
        }
    }

    #[test]
    fn graphemes_can_span_the_gap() {
        let mut buffer = Buffer::new();
        buffer.insert(0, "e\u{301}🙂");
        buffer.mov_gap(1); // Between the base letter and combining accent.
        assert_eq!(buffer.grapheme_to_byte(0), Some(0));
        assert_eq!(buffer.grapheme_to_byte(1), Some(3));
        assert_eq!(buffer.grapheme_to_byte(2), Some(7));
        assert_eq!(buffer.grapheme_to_byte(3), None);
    }

    #[test]
    #[should_panic(expected = "invalid UTF-8 byte index")]
    fn insertion_rejects_split_codepoint() {
        let mut buffer = Buffer::new();
        buffer.insert(0, "é");
        buffer.insert(1, "x");
    }

    #[test]
    fn layout_uses_graphemes_and_byte_ranges() {
        let mut buffer = Buffer::new();
        buffer.insert(0, "ée\u{301}👩‍💻\t中\nZ");
        let (cursor, content) = buffer.layout(2);
        assert_eq!(cursor, 2);
        let mut rendered = String::new();
        let mut cursor_text = None;
        for fragment in &content.frags {
            if let FragmentText::Indx { start, end, .. } = fragment.text {
                let text = &content.text[start..end];
                rendered.push_str(text);
                if matches!(fragment.style, Style::Cursor) {
                    cursor_text = Some(text);
                }
            }
        }
        assert_eq!(rendered, "ée\u{301}👩‍💻中Z");
        assert_eq!(cursor_text, Some("👩‍💻"));
        assert_eq!(buffer.layout(usize::MAX).0, 7);
    }

    #[test]
    fn insert_small() {
        let mut ebuf = Buffer::new();

        ebuf.insert(0, "Blh");
        assert_eq!(&ebuf.gap_buf[..3], b"Blh");
        assert_eq!(ebuf.gap_indx, 3);

        ebuf.insert(2, "aaaa");
        let res: String = ebuf.chars().collect();
        assert_eq!(&res, "Blaaaah");
    }
}
