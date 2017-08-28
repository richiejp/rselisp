use std::io::stdin;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
enum Inner {
    Int(i32),
    Str(String),
    Sym(String),
    Sxp(Sexp),
}

impl Inner {
    fn ref_sxp(&mut self) -> &mut Sexp {
        if let &mut Inner::Sxp(ref mut sxp) = self {
            sxp
        } else {
            panic!("Not an Sexp");
        }
    }
}

#[derive(Debug)]
struct Sexp {
    delim: char,
    lst: Vec<Inner>,
}

impl Sexp {
    fn root() -> Sexp {
        Sexp {
            delim: 'R',
            lst: Vec::new(),
        }
    }
    
    fn new(delim: char) -> Sexp {
        Sexp {
            delim: delim,
            lst: Vec::new(),
        }
    }
    
    fn push(&mut self, child: Inner) {
        self.lst.push(child);
    }

    fn new_inner_sxp(&mut self, delim: char) -> &mut Sexp {
        self.lst.push(Inner::Sxp(Sexp::new(delim)));
        self.lst.last_mut().unwrap().ref_sxp()
    }
}

#[derive(Debug)]
enum Token {
    Spc,
    Qot,
    Lbr(char),
    Rbr(char),
    Atm(String),
    Str(String)
}

struct Lsp {
    ast: Sexp,
}

impl Lsp {
    fn tok_spc(itr: &mut Peekable<Chars>) -> Result<Token, &'static str> {
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

    fn tok_str(q: char, itr: &mut Peekable<Chars>) -> Result<Token, &'static str> {
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

    fn tok_atom(l: char, itr: &mut Peekable<Chars>) -> Result<Token, &'static str> {
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

    fn tokenize(input: &String) -> Result<Vec<Token>, &'static str> {
        let mut toks = Vec::<Token>::new();
        let mut itr = input.chars().peekable();
        
        while let Some(c) = itr.next() {
            let res = match c {
                ' ' | '\t' | '\n' | '\r' => Lsp::tok_spc(&mut itr),
                '(' | '{' | '[' => Ok(Token::Lbr(c)),
                ')' | '}' | ']' => Ok(Token::Rbr(c)),
                '"' => Lsp::tok_str('"', &mut itr),
                '\'' => {
                    if Some(&'(') == itr.peek() {
                        Ok(Token::Qot)
                    } else {
                        Lsp::tok_str('\'', &mut itr)
                    }
                },
                _ => Lsp::tok_atom(c, &mut itr)
            };
            match res {
                Ok(Token::Spc) => (),
                Ok(tok) => toks.push(tok),
                Err(e) => return Err(e),
            }
        }

        Ok(toks)
    }

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

    fn read(input: &String) -> Result<Sexp, String> {
        match Lsp::tokenize(input) {
            Ok(toks) => {
                //Iterator
                let mut itr = toks.iter().peekable();
                //AST root
                let mut tree = Sexp::root();

                {
                    let mut anc = vec![&mut tree];

                    while let Some(t) = itr.next() {
                        let cur = anc.pop().unwrap();

                        match t {
                            &Token::Lbr(c) => unsafe {
                                let cur = cur as *mut Sexp;
                                let nsxp = (&mut *cur).new_inner_sxp(c);
                                anc.push(&mut *cur);
                                anc.push(nsxp);
                            },
                            &Token::Rbr(c) => {
                                if cur.delim == 'R' {
                                    return Err(format!("There are more '{}' than '{}'", c, Lsp::inv_brk(c)));
                                } else if cur.delim != Lsp::inv_brk(c) {
                                    return Err(format!("Mismatch '{}' with '{}'", cur.delim, c));
                                }
                            },
                            &Token::Atm(ref s) => {
                                if let Ok(i) = i32::from_str_radix(s, 10) {
                                    cur.push(Inner::Int(i));
                                } else {
                                    cur.push(Inner::Sym(s.to_owned()));
                                }
                                anc.push(cur);
                            },
                            &Token::Str(ref s) => {
                                cur.push(Inner::Str(s.to_owned()));
                                anc.push(cur);
                            },
                            &Token::Qot => return Err(format!("Quotes not implemented")),
                            &Token::Spc => panic!("Space token not supported"),
                        }
                    }
                }
                Ok(tree)
            },
            Err(e) => Err(String::from(e)),
        }
    }
}


fn main() {
    println!("'(rselisp repl v0.0)");

    loop {
        let mut line = String::new();
        match stdin().read_line(&mut line) {
            Ok(_) => {
                match Lsp::read(&line) {
                    Ok(sexp) => println!("{:?}", sexp),
                    Err(e) => println!("ERROR: {}", e),
                }
            },
            Err(e) => { println!("FATAL: {}", e); break; },
        }
    }
}
