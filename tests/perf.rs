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

#[bench]
fn cons(b: &mut Bencher) {
    let mut lsp = Lsp::new();
    let src = r#"
(defalias 'repeat
  (lambda (a c)
    (if (eq c 0)
	a
      (cons a (repeat a (- c 1))))))

(defalias 'prn-lst
  (lambda (l)
    (if (listp l) (progn
		    (print (car l))
		    (prn-lst (cdr l)))
      (print l))))

(prn-lst (repeat 'a 100))
"#.to_owned();
    let ast = lsp.read(&src).unwrap();

    b.iter(|| lsp.eval(&ast));    
}
