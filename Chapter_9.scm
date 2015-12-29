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

; We include this function here only because a-pair? use it.
(define atom?
	(lambda (x)
		(and (not (pair? x)) (not (null? x)))
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
; Otherwise, let it be. And it is a total function.
(define align
	(lambda (pora)
		(cond
			((atom? pora) pora)
			((a-pair? (first pora)) (align (shift pora)))
			(else (build (first pora) (align (second pora))))
		)
	)
)

; length* counts the number of atoms in align's arguments in a weird way,
; which means it can only handle pairs correctly.
; For example, (length* '(a (b c d))) --> 3;
;              (length* '((a b) (c d) e) --> 4;
;              (length* '((a b) (c (d e))) --> 5.
(define length*
	(lambda (pora)
		(cond
			((atom? pora) 1)
			(else (+ (length* (first pora)) (length* (second pora))))
		)
	)
)

; weight* is the same as length* but doubles the first component in the pair.
; For example, (weight* '((a b) c)) --> 7;
;              (weight* '(a (b c))) --> 5.
(define weight*
	(lambda (pora)
		(cond
			((atom? pora) 1)
			(else (+ (* (weight* (first pora)) 2) (weight* (second pora))))
		)
	)
)

; revpair reverses the two components of a pair,
; used in revised revrel function.
; Exactly the same function from Chapter 7.
(define revpair
	(lambda (pair)
		(build (second pair) (first pair))
	)
)

; shuffle is the same as align but uses revpair instead of shift,
; which is a partial function (try (shuffle '((a b) (c d)))).
(define shuffle
	(lambda (pora)
		(cond
			((atom? pora) pora)
			((a-pair? (first pora)) (shuffle (revpair pora)))
			(else (build (first pora) (shuffle (second pora))))
		)
	)
)

; A more concise version of function one?.
; Exactly the same function from Chapter 4.
(define one?
	(lambda (n)
		(= n 1)
	)
)

; Exactly the same function from Chapter 4.
(define add1
	(lambda (n)
		(+ n 1)
	)
)

; This is a function to check the correctness of The Collatz conjecture (Kakutani's problem).
(define C
	(lambda (n)
		(cond
			((one? n) 1)
			(else
				(cond
					((even? n) (C (/ n 2)))
					(else (C (add1 (* 3 n))))
				)
			)
		)
	)
)

; Exactly the same function from Chapter 4.
(define sub1
	(lambda (n)
		(- n 1)
	)
)

; A is an Ackermann function, which is an total computable function that is not primitive recursive.
; For example, (A 4 3) will not have a final certain answer.
(define A
	(lambda (n m)
		(cond
			((zero? n) (add1 m))
			((zero? m) (A (sub1 n) 1))
			(else (A (sub1 n) (A n (sub1 m))))
		)
	)
)

; s/c stands for short circuit, which is a total function.
(define s/c
	(lambda (L)
		(and #f (eternity L))
		; (and (eternity L) #f)
		; But with the statement above, the program will not stop.
	)
)

; Standard length function.
(define length
	(lambda (l)
		(cond
			((null? l) 0)
			(else (add1 (length (cdr l))))
		)
	)
)

; length_0 function.
(lambda (l)
	(cond
		((null? l) 0)
		(else (add1 (eternity (cdr l))))
	)
)

; The first version of length_<=1 function.
(lambda (l)
	(cond
		((null? l) 0)
		(else (add1 (length_0 (cdr l))))
	)
)

; The second version of length_<=1 function (replace length_0 with its definition).
(lambda (l)
	(cond
		((null? l) 0)
		(else
			(add1
				(
					(lambda (l)
						(cond
							((null? l) 0)
							(else (add1 (eternity (cdr l))))
						)
					) (cdr l) ; (length_0 (cdr l))
				)
			)
		)
	)
)

; The first version of length_<=2 function.
(lambda (l)
	(cond
		((null? l) 0)
		(else (add1 (length_<=1 (cdr l))))
	)
)

; The second version of length_<=2 function (replace length_<=1 with its definition).
(lambda (l)
	(cond
		((null? l) 0)
		(else
			(add1
				(
					(lambda (l)
						(cond
							((null? l) 0)
							(else
								(add1
									(
										(lambda (l)
											(cond
												((null? l) 0)
												(else (add1 (eternity (cdr l))))
											)
										) (cdr l) ; (length_0 (cdr l))
									)
								)
							)
						)
					) (cdr l) ; (length_<=1 (cdr l))
				)
			)
		)
	)
)

; This is actually a different way to implement length_0 function.
(
	(lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (length (cdr l))))
			)
		)
	) eternity ; eternity here acts as an argument (length).
)

; Rewrite length_<=1 in the same style using length_0.
(
	(lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (length (cdr l))))
			)
		)
	) length_0 ; length_0 here acts as an argument (length).
)

; Rewrite length_<=1 in the same style replacing length_0 with its definition.
; All fs and gs can be replace by length as we used before.
(
	(lambda (f)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (f (cdr l))))
			)
		)
	)
	; The code bewtween two asterisks (length_0) acts as an argument (f).
	; *
	(
		(lambda (g)
			(lambda (l)
				(cond
					((null? l) 0)
					(else (add1 (g (cdr l))))
				)
			)
		) eternity
	)
	; *
)

; Rewrite length_<=2 in the same style using length_<=1.
(
	(lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (length (cdr l))))
			)
		)
	) length_<=1 ; length_<=1 here acts as an argument (length).
)

; Rewrite length_<=2 in the same style replacing length_0 with its definition.
(
	(lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (length (cdr l))))
			)
		)
	)
	; The code bewtween two asterisks (length_<=1) acts as an argument (length).
	; *
	(
		(lambda (length)
			(lambda (l)
				(cond
					((null? l) 0)
					(else (add1 (length (cdr l))))
				)
			)
		)
		(
			(lambda (length)
				(lambda (l)
					(cond
						((null? l) 0)
						(else (add1 (length (cdr l))))
					)
				)
			) eternity
		)
	)
	; *
)

; Simplify the rewritten version of length_0 function.
(
	(lambda (mk-length)
		(mk-length eternity)
	)
	; The code bewtween two asterisks acts as an argument (mk-length).
	; *
	(lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (length (cdr l))))
			)
		)
	)
	; *
)

; Simplify the rewritten version of length_<=1 function.
(
	(lambda (mk-length)
		(mk-length ; (mk-length length_0) --> length_<=1
			(mk-length eternity) ; length_0
			; We can use recursively simply because all length_*
			; functions share the same pattern, which will be passed by mk-length argument.
		)
	)
	(lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (length (cdr l))))
			)
		)
	)
)

; Simplify the rewritten version of length_<=2 function.
(
	(lambda (mk-length)
		(mk-length ; (mk-length length_<=1) --> length_<=2
			(mk-length ; (mk-length length_0) --> length_<=1
				(mk-length eternity) ; length_0
			)
		)
	)
	(lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (length (cdr l))))
			)
		)
	)
)

; Simplify the rewritten version of length_<=3 function.
(
	(lambda (mk-length)
		(mk-length ; (mk-length length_<=2) --> length_<=3
			(mk-length ; (mk-length length_<=1) --> length_<=2
				(mk-length ; (mk-length length_0) --> length_<=1
					(mk-length eternity) ; length_0
				)
			)
		)
	)
	(lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (length (cdr l))))
			)
		)
	)
)

; This is still length_0 function.
(
	(lambda (mk-length)
		(mk-length mk-length)
		; We replace eternity with mk-length, since nobody cares what function
		; we pass to mk-length (theoretically eternity part will never be run).
	)
	(lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (length (cdr l))))
				; When l is not empty, the statement does not work at here (because of lack of arugments),
				; which has the same effect as we use eternity function.
			)
		)
	)
)

; For the last version of length_0 function, we can even replace length with mk-length.
(
	(lambda (mk-length)
		(mk-length mk-length)
	)
	(lambda (mk-length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (mk-length (cdr l))))
			)
		)
	)
)

; We rewrite length_<=1 based on the newest version of length_0.
(
	(lambda (mk-length)
		(mk-length mk-length)
	)
	(lambda (mk-length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 ((mk-length eternity) (cdr l))))
			)
		)
	)
)

; We rewrite length function based on the newest version of length_<=1.
; It keeps adding recursive uses by passing mk-length to itself, just
; as it is about to expire.
(
	(lambda (mk-length)
		(mk-length mk-length)
	)
	(lambda (mk-length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 ((mk-length mk-length) (cdr l))))
			)
		)
	)
)

; We extract a new application of mk-length to iteself and call it length.
(
	(lambda (mk-length)
		(mk-length mk-length)
	)
	(lambda (mk-length)
		(lambda (length)
			(lambda (l)
				(cond
					((null? l) 0)
					(else (add1 (length (cdr l))))
				)
			) (mk-length mk-length)
		)
	)
)
