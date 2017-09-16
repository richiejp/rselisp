(defalias 'fib
  (lambda (a)
    (if (eq a 1)
	1
      (if (eq a 2)
	  2
	(+ (fib (- a 1)) (fib (- a 2)))))))

(defalias 'fibs
  (lambda (a)
    (if (eq a 0)
	nil
      (progn
	(print (fib a))
	(fibs (- a 1))))))

(defalias 'hello-world
  (lambda () (print "Hello, World!")))

(hello-world)
(print "fibs: ")
(fibs 10)
