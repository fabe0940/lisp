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

(defun ncons (LL)
	(if (consp LL)
		(+ 1 (ncons (car LL)) (ncons (cdr LL)))
		0
	)
)

(defun nvowels_isvowel (ch)
	(cond
		((eq ch #\A) 1)
		((eq ch #\a) 1)
		((eq ch #\E) 1)
		((eq ch #\e) 1)
		((eq ch #\I) 1)
		((eq ch #\i) 1)
		((eq ch #\O) 1)
		((eq ch #\o) 1)
		((eq ch #\U) 1)
		((eq ch #\u) 1)
		(t 0)
	)
)

(defun nvowels_help (L)
	(cond
		((not (listp L)) 0)
		((null L) 0)
		(t (+ (nvowels_isvowel(car L)) (nvowels_help (cdr L))))
	)
)

(defun nvowels (s)
	(cond
		((not (stringp s)) 0)
		(t (nvowels_help (coerce s 'list)))
	)
)

(defun bit2list_val (ch)
	(cond
		((eq ch #\1) t)
		(t nil)
	)
)

(defun bit2list_help (L)
	(cond
		((not (listp L)) nil)
		((null L) nil)
		(t (cons (bit2list_val (car L)) (bit2list_help (cdr L))))
	)
)

(defun bit2list (s)
	(cond
		((not (stringp s)) nil)
		(t (bit2list_help (coerce s 'list)))
	)
)
