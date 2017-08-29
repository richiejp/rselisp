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

use std::str::Chars;
use std::iter::Peekable;

#[derive(Debug)]
pub enum Token {
    Spc,
    Qot,
    Lbr(char),
    Rbr(char),
    Atm(String),
    Str(String)
}

pub trait Tokenizer {
    fn inv_brk(c: char) -> char {
        match c {
            '(' => ')',
            ')' => '(',
            '[' => ']',
            ']' => '[',
            '{' => '}',
            '}' => '{',
            c => panic!("Unrecognised bracket char '{}'", c),
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

    fn tok_atom(&self, l: char, itr: &mut Peekable<Chars>) -> Result<Token, &'static str> {
        let mut s = String::new();
        s.push(l);

        while let Some(&c) = itr.peek() {
            match c {
                ' ' | '\t' | '\n' | '\r' | '(' | '{' | '[' | ']' | '}' | ')'
                    => return Ok(Token::Atm(s)),
                _ => {s.push(c); itr.next()},
            };
        }

        Err("EOF while tokenizing atom")
    }

    fn tokenize(&self, input: &String) -> Result<Vec<Token>, &'static str> {
        let mut toks = Vec::<Token>::new();
        let mut itr = input.chars().peekable();
        
        while let Some(c) = itr.next() {
            let res = match c {
                ' ' | '\t' | '\n' | '\r' => self.tok_spc(&mut itr),
                '(' | '{' | '[' => Ok(Token::Lbr(c)),
                ')' | '}' | ']' => Ok(Token::Rbr(c)),
                '"' => self.tok_str('"', &mut itr),
                '\'' => {
                    if Some(&'(') == itr.peek() {
                        Ok(Token::Qot)
                    } else {
                        self.tok_str('\'', &mut itr)
                    }
                },
                _ => self.tok_atom(c, &mut itr)
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
