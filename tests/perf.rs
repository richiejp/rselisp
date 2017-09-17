#![feature(test)]
extern crate test;
extern crate rselisp;

use test::Bencher;
use rselisp::{Lsp, Inner};

#[bench]
fn fib(b: &mut Bencher) {
    let mut lsp = Lsp::new();
    let src = r#"
(defalias 'fib
  (lambda (a)
    (if (eq a 1)
	1
      (if (eq a 2)
	  2
	(+ (fib (- a 1)) (fib (- a 2)))))))

(fib 20)
"#.to_owned();
    let ast = lsp.read(&src).unwrap();

    b.iter(|| assert_eq!(Ok(Inner::Int(10946)), lsp.eval(&ast)));
}
