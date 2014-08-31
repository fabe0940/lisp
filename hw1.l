#!/usr/local/bin/clisp
; Mason Fabel, CS 210 HW #1, Lisp Assignment

(defun foo (x)
	(if (integerp x)
		(if (<= x 1)
			1
			(* (foo (- x 1)) (/ x 2)))))

(defun sigsum (L)
	(if (and (not (null L)) (numberp (car L)))
		(+ (sigsum (cdr L))
		(if (> (car L) 0)
			1
			(if (< (car L) 0)
				-1
				0)))
	0))

(defun squaredevens (L)
	(if (null L)
		nil
		(cons (print (if (and (integerp (car L)) (evenp (car L)))
			(* (car L) (car L))
			nil))
		(squaredevens (cdr L)))))

(quit)
