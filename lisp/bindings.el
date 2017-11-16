(define-key global-map "\C-`" ''exit)
(define-key global-map "\M-x"
  '(lambda () (print "Wouldn't it be great if this actually did something")))
(define-key global-map "\C-f" '(forward-char))
(define-key global-map "\C-b" '(backward-char))
