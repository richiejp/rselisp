extern crate criterion;
extern crate rselisp;

use criterion::{criterion_group, criterion_main, Criterion};
use rselisp::{Lsp, LispObj};

fn fib(c: &mut Criterion) {
    let mut lsp = Lsp::new();
    let src = r#"
(fset 'fib
  '(lambda (a)
    (if (eq a 1)
	1
      (if (eq a 2)
	  2
	(+ (fib (- a 1)) (fib (- a 2)))))))

(fib 20)
"#.to_owned();
    let ast = lsp.read(&src).unwrap();

    c.bench_function("fib", |b| {
        b.iter(|| assert_eq!(Ok(LispObj::Int(10946)), lsp.eval(&ast)))
    });
}

fn cons(c: &mut Criterion) {
    let mut lsp = Lsp::new();
    let src = r#"
(fset 'repeat
  '(lambda (a c)
    (if (eq c 0)
	a
      (cons a (repeat a (- c 1))))))

(fset 'add1
  '(lambda (l)
    (if (listp l)
	(cons (+ 1 (car l)) (add1 (cdr l)))
      l)))

(add1 (repeat 1 100))
"#.to_owned();
    let ast = lsp.read(&src).unwrap();

    c.bench_function("cons", |b| b.iter(|| lsp.eval(&ast)));
}

criterion_group!(benches, fib, cons);
criterion_main!(benches);
