use std::fs::File;
use std::io::prelude::*;

pub struct Buffer {
    frntxt: String,
    bktxt: String,
    
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            frntxt: String::with_capacity(8192),
            bktxt: String::with_capacity(8192),
        }
    }

    pub fn find_file(&mut self, name: &str) -> Result<(), String> {
        match File::open(name) {
            Ok(mut file) => {
                if let Err(e) = file.read_to_string(&mut self.bktxt) {
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
}

pub struct Window {
}

pub trait Frame {
}

pub struct OrbFrame {
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_file() {
        let fname = "lisp/demo.el";
        let mut ebuf = Buffer::new();

        assert_eq!(Ok(()), ebuf.find_file(fname));
        assert!(ebuf.bktxt.len() > 0);
    }
}
