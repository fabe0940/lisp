#!/usr/local/bin/clisp
(defun foo (x) (if (integerp x) (if (<= x 1) 1 (* (foo (- x 1)) (/ x 2)))))
(defun sigsum (L) (if (and (not (null L)) (numberp (car L))) (+ (sigsum (cdr L)) (if (> (car L) 0) 1 (if (< (car L) 0) -1 0))) 0))
(quit)
