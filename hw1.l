; Mason Fabel, CS 210 HW #1, Lisp Assignment

(defun foo (x)
	(cond
		((not (integerp x)) nil)
		((<= x 1) 1)
		(t (* (foo (- x 1)) (/ x 2)))
	)
)

(defun sigsum (L)
	(cond
		((null L) 0)
		(t
			(+
				(cond
					((not (numberp (car L))) 0)
					((> (car L) 0) 1)
					((< (car L) 0) -1)
					(t 0)
				)
				(sigsum (cdr L))
			)
		)
	)
)

(defun squaredevens (L)
	(cond
		((null L) nil)
		(t
			(cons
				(let ((val (car L)))
					(cond
						((not (integerp val)) nil)
						((not (evenp val)) nil)
						(t (* val val))
					)
				)
				(squaredevens (cdr L))
			)
		)
	)
)

(defun polyval (L x)
	(cond
		((null L) 0)
		((not (listp L)) nil)
		((not (listp (car L))) nil)
		((null (car L)) nil)
		((not (eq 2 (length (car L)))) nil)
		((not (numberp x)) nil)
		(t
			(let ((coef (caar L)) (pow (car (cdr (car L)))))
				(+ (* coef (expt x pow)) (polyval (cdr L) x))
			)
		)
	)
)

(defun hasp_help_findinlist (L n)
	(cond
		((null L) nil)
		(t (or (eq (car L) n) (hasp_help_findinlist (cdr L) n)))
	)
)

(defun hasp (L n)
	(cond
		((not (listp L)) nil)
		((null L) nil)
		((not (numberp n)) nil)
		(t
			(cond
				((listp (car L))
					(or (hasp (car L) n) (hasp (cdr L) n))
				)
				(t
					(or (eq (car L) n) (hasp (cdr L) n))
				)
			)
		)
	)
)
