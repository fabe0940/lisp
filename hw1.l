;;; Mason Fabel, CS 210 HW #1, Lisp Assignment

;; FOO(X)
; Returns NIL if X is not an integer.
; Returns 1 if X <= 1.
; Returns FOO(X - 1) * (X / 2) if X > 1.
(defun foo (x)
	(cond
		; precondition
		((not (integerp x)) nil)
		; base case
		((<= x 1) 1)
		; recursion
		(t (* (foo (- x 1)) (/ x 2)))
	)
)

;; SIGSUM(L)
; Returns the sum of the signs of the atoms of a list.
; Returns NIL if L is not a list.
(defun sigsum (L)
	(cond
		; precondition
		((not (listp L)) nil)
		; base case
		((null L) 0)
		; recursion
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

;; SQUAREDEVENS_HELPER(A)
; Returns (* A A) if atom A is an even integer.
; Returns NIL otherwise.
(defun squaredevens_helper (a)
	(cond
		((and (integerp a) (evenp a)) (* a a))
		(t nil)
	)
)

;; SQUAREDEVENS(L)
; Returns a list of the same length as L, where each
; element's value is the return value of
; SQUAREDEVENS_HELPER called on the item in L at the
; same index.
(defun squaredevens (L)
	(cond
		; preconditions
		((null L) nil)
		; recursion
		(t (mapcar 'squaredevens_helper L))
	)
)

;; POLYVAL(L, X)
; Returns the value of a polynomial evaluated at X.
; Returns NIL if the polynomial is in an invalid form,
; or if X is not a number.
(defun polyval (L x)
	(cond
		; base case
		((null L) 0)
		; preconditions
		((not (listp L)) nil)
		((not (listp (car L))) nil)
		((null (car L)) nil)
		((not (eq 2 (length (car L)))) nil)
		((not (numberp x)) nil)
		; recursion
		(t
			(let ((coef (caar L)) (pow (cadar L)))
				(+ (* coef (expt x pow)) (polyval (cdr L) x))
			)
		)
	)
)

;; HASP(L, N)
; Returns whether N is an atom in L, an arbitrarily
; nested list of lists.
(defun hasp (L n)
	(cond
		; base case
		((not (listp L)) nil)
		((null L) nil)
		; precondition
		((not (numberp n)) nil)
		; recursion
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

;; NCONS(LL)
; Returns the number of cons cells in LL, an arbitrarily
; nested list of lists.
(defun ncons (LL)
	(if (consp LL)
		; return 1 + # of cons in the car + # of cons in the cdr
		(+ 1 (ncons (car LL)) (ncons (cdr LL)))
		; not a cons cell; return 0
		0
	)
)

;; NVOWELS_ISVOWEL(CH)
; Returns 1 if CH is a vowel.
; Returns 0 otherwise.
(defun nvowels_isvowel (ch)
	(cond
		; vowels both cases
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
		; other characters
		(t 0)
	)
)

;; NVOWELS_HELP(L)
; Returns the number of vowels in L, where L is a list
; of individual characters.
; The vowelhood of each character is determined by
; NVOWELS_ISVOWEL.
(defun nvowels_help (L)
	(cond
		; precondition
		((not (listp L)) 0)
		; base case
		((null L) 0)
		; recursion
		(t (+ (nvowels_isvowel(car L)) (nvowels_help (cdr L))))
	)
)

;; NVOWELS(S)
; Returns the number of vowels in string S.
; Breaks the string down into a list of characters and
; hands it off to NVOWELS_HELP.
(defun nvowels (s)
	(cond
		; preconditions
		((not (stringp s)) 0)
		; hand off to the helper
		(t (nvowels_help (coerce s 'list)))
	)
)

;; BIT2LIST_VAL(CH)
; Returns T if CH is character #\1.
; Returns NIL otherwise.
(defun bit2list_val (ch)
	(cond
		((eq ch #\1) t)
		(t nil)
	)
)

;; BIT2LIST_HELP(L)
; Returns a list the same length as list L.
; Each value in the list is T if the corresponding value in L
; is #\1.
; Otherwise, the element is NIL.
(defun bit2list_help (L)
	(cond
		; preconditions
		((not (listp L)) nil)
		; base case
		((null L) nil)
		; recursion
		(t (cons (bit2list_val (car L)) (bit2list_help (cdr L))))
	)
)

;; BIT2LIST(S)
; Returns a list the same length as string S.
; Each value in the list is T is the corresponding character in S
; is '1'.
; Otherwise, the value is NIL.
(defun bit2list (s)
	(cond
		; preconditions
		((not (stringp s)) nil)
		; hand off to the helper
		(t (bit2list_help (coerce s 'list)))
	)
)
