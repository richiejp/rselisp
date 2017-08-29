use std::io::stdin;

extern crate rselisp;

use rselisp::Lsp;

fn main() {
    let mut lsp = Lsp::new();
    
    println!("'(rselisp repl v0.0)");

    loop {
        let mut line = String::new();
        match stdin().read_line(&mut line) {
            Ok(_) => {
                match lsp.read(&line) {
                    Ok(sexp) => match lsp.eval(&sexp) {
                        Ok(resexp) => println!("-> {:?}", resexp),
                        Err(e) => println!("EVAL ERROR: {}", e),
                    },
                    Err(e) => println!("READ ERROR: {}", e),
                }
            },
            Err(e) => { println!("I/O ERROR: {}", e); break; },
        }
    }
}
