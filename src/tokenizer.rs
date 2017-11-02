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

use std::error::Error;
use std::str::Chars;
use std::iter::Peekable;
use symbols::{Atom, AtomRegistry};

#[derive(Debug, PartialEq, Eq)]
pub struct Number {
    pub significand: i32,
    //base: u8,
    //exponent: u8,
}

impl Number {
    fn default() -> Number {
        Number {
            significand: 0,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Spc,
    Qot,
    Lbr(char),
    Rbr(char),
    Atm(Atom),
    Str(String),
    Num(Number)
}

pub trait Tokenizer {
    fn atoms(&mut self) -> &mut AtomRegistry;

    fn inv_brk(c: char) -> char {
        match c {
            '(' => ')',
            ')' => '(',
            '[' => ']',
            ']' => '[',
            '{' => '}',
            '}' => '{',
            c => c,
        }
    }

    fn tok_spc(&self, itr: &mut Peekable<Chars>) -> Result<Token, &'static str> {
        loop {
            if let Some(&c) = itr.peek() {
                match c {
                    ' ' | '\t' | '\n' | '\r' => itr.next(),
                    _ => break Ok(Token::Spc),
                };
            } else {
                break Ok(Token::Spc);
            }
        }
    }

    fn tok_str(&self, q: char, itr: &mut Peekable<Chars>) -> Result<Token, &'static str> {
        let mut s = String::new();

        while let Some(c) = itr.next() {
            match c {
                '\\' => match itr.next() {
                    Some(c) if c == q => s.push(c),
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('r') => s.push('\r'),
                    Some('\\') => s.push('\\'),
                    Some(l) => {
                        s.push(c);
                        s.push(l);
                    },
                    None => break,
                },
                _ if c == q => return Ok(Token::Str(s)),
                l => s.push(l),
            }
        }

        Err("EOF while tokenizing string")
    }

    fn tok_atom_or_num(&mut self, l: char, itr: &mut Peekable<Chars>)
                -> Result<Token, &'static str> {
        let mut s = String::new();
        let mut num = Number::default();
        // let mut seen_e = false;
        // let mut seen_dot = false;
        // let mut seen_sign = false;
        s.push(l);

        match l {
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                while let Some(&c) = itr.peek() {
                    match c {
                        ' ' | '\t' | '\n' | '\r' | '(' | '{' | '[' | ']' | '}' | ')' => {
                            return match s.parse::<i32>() {
                                Ok(i) => {
                                    num.significand = i;
                                    Ok(Token::Num(num))
                                },
                                Err(e) => {
                                    panic!("Rust can not parse '{}' into an integer: {}",
                                           s, e)
                                },
                            };
                        },
                        '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                            s.push(c);
                            itr.next()
                        },
                        _ => {
                            s.push(c);
                            itr.next();
                            break;
                        },
                    };
                }
            },
            _ => (),
        }

        while let Some(&c) = itr.peek() {
            match c {
                ' ' | '\t' | '\n' | '\r' | '(' | '{' | '[' | ']' | '}' | ')' => {
                    return Ok(Token::Atm(self.atoms().atomize_mv(s)))
                },
                _ => {
                    s.push(c);
                    itr.next();
                },
            }
        }

        Err("EOF while tokenizing atom")
    }

    fn tokenize(&mut self, input: &String) -> Result<Vec<Token>, &'static str> {
        let mut toks = Vec::<Token>::new();
        let mut itr = input.chars().peekable();

        while let Some(c) = itr.next() {
            let res = match c {
                ' ' | '\t' | '\n' | '\r' => self.tok_spc(&mut itr),
                '(' | '{' | '[' => Ok(Token::Lbr(c)),
                ')' | '}' | ']' => Ok(Token::Rbr(c)),
                '"' => self.tok_str('"', &mut itr),
                '\'' => Ok(Token::Qot),
                _ => self.tok_atom_or_num(c, &mut itr)
            };
            match res {
                Ok(Token::Spc) => (),
                Ok(tok) => toks.push(tok),
                Err(e) => return Err(e),
            }
        }

        Ok(toks)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use symbols;

    struct TestTokenizer {
        atoms: AtomRegistry,
    }
    impl TestTokenizer {
        fn new() -> TestTokenizer {
            TestTokenizer {
                atoms: AtomRegistry::with_capacity(5),
            }
        }
    }
    impl Tokenizer for TestTokenizer {
        fn atoms(&mut self) -> &mut AtomRegistry {
            &mut self.atoms
        }
    }

    #[test]
    fn atom() {
        let mut nizer = TestTokenizer::new();
        let lisp = "(atom nil lambda [let])";

        let res = nizer.tokenize(&lisp.into()).unwrap();
        let reg = nizer.atoms();
        assert_eq!(res[1], Token::Atm(reg.atomize("atom")));
        assert_eq!(res[2], Token::Atm(symbols::NIL));
        assert_eq!(res[3], Token::Atm(symbols::LAMBDA));
        assert_eq!(res[5], Token::Atm(reg.atomize("let")));
    }

    #[test]
    fn number() {
        let mut nizer = TestTokenizer::new();
        let lisp = "(nil lambda 01234 [let])";

        let res = nizer.tokenize(&lisp.into()).unwrap();
        assert_eq!(res[3], Token::Num(Number { significand: 1234 } ));
    }
}
