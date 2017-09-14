This is currently a crude Lisp interpreter written in Rust and loosely based
on Emacs Lisp. To try it out do the following (assuming you have installed
cargo):

```
$ git clone https://github.com/richiejp/rselisp.git
$ cd rselisp
$ cargo run
```

Not much is currently possible, but here is some example I/O

```lisp
(defalias 'fib (lambda (a) (if (eq a 1) 1 (if (eq a 2) 2 (+ (fib (- a 1)) (fib (- a 2)))))))
-> fib
(fib 10)
-> 89
(defalias 'fibs (lambda (a) (if (eq a 0) nil (progn (print (fib a)) (fibs (- a 1
)))))
-> fibs
(fibs 10)
(89)
(55)
(34)
(21)
(13)
(8)
(5)
(3)
(2)
(1)
-> nil
(exit)
'(Good bye!)
```
