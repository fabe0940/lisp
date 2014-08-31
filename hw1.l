#!/usr/local/bin/clisp
(defun foo (x) (if (integerp x) (if (<= x 1) 1 (* (foo (- x 1)) (/ x 2)))))
(quit)
