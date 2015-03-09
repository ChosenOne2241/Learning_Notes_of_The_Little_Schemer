; CHAPTER NINE
; Author: '(Yongzhen R.)


; Pick the nth S-expression from a lat.
; n should be bigger than 0; the length of lat cannot be smaller than n.
; Exactly the same function from Chapter 4.
(define pick
	(lambda (n lat)
		(cond
			((zero? (sub1 n)) (car lat))
			(#t (pick (sub1 n) (cdr lat)))
		)
	)
)

; Partial function.
(define looking
	(lambda (a lat)
		(keeping-looking a (pick 1 lat) lat)
	)
)

; keeping-looking checks elements in a lat; if current one is a number,
; then refers to sorn-th element in i, until it finds a symbol and
; checks if it is equal to a (return #t / #f).
; If an atom visited is a number, in any case, the number cannot be bigger than
; the length of lat since pick function is used here.
; sorn stands for symbol or number.
(define keeping-looking
	(lambda (a sorn lat)
		(cond
			((number? sorn)	(keeping-looking a (pick sorn lat) lat))
			(else (eq? sorn a))
		)
	)
)

; eternity is the most partial function, which will infinite recursion.
; DO NOT TRY TO RUN IT.
(define eternity
	(lambda (x)
		(eternity x)
	)
)

; first gets the first element from a list.
; Exactly the same function from Chapter 7.
(define first
	(lambda (L)
		(car L)
	)
)

; second gets the second element from a list.
; Exactly the same function from Chapter 7.
(define second
	(lambda (L)
		(car (cdr L))
	)
)

; The function builds a pair with two S-expressions.
; Exactly the same function from Chapter 7.
(define build
	(lambda (s1 s2)
		(cons s1 (cons s2 '())) ; Don't forget to cons '().
	)
)

; The function takes a pair whose first component is a pair and builds a pair
; by shifting the second part of the first component into the second component.
(define shift
	(lambda (pair)
		(build (first (first pair)) (build (second (first pair)) (second pair)))
	)
)

; Pair is a list containing just two S-expressions, such as (a (a b)), (3 4).
; a-pair? checks if L is a pair.
; Exactly the same function from Chapter 7.
(define a-pair?
	(lambda (L)
		(cond
			; An S-expression.
			((null? L) #f) ; If it is empty.
			; An atom or a non-empty list.
			((atom? L) #f) ; If it is an atom.
			; A non-empty list with one or two or more elements.
			((null? (cdr L)) #f) ; If it contains only one element.
			; A non-empty list with two or more elements.
			((null? (cdr (cdr L))) #t)
			; If the third element is null (it contains exactly two elements).
			(else #f) ; A non-empty list with more than two elements.
		)
	)
)

; align goes through a pair of two elements (two pairs / two atoms / a pair and an atom)
; and do shift opeartion on it if the first element of the pair is also a pair.
; Otherwise, let it be.
(define align
	(lambda (pora)
		(cond
			((atom? pora) pora)
			((a-pair? (first pora)) (align (shift pora)))
			(else (build (first pora) (align (second pora))))
		)
	)
)
