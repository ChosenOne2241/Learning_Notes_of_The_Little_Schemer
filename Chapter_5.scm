; CHAPTER FIVE
; Author: Yongzhen Ren

; All arguments new, old and a are atoms; L is a list.
; L can be a list of any type.
; e.g: a list of purely lists or a lat or a list containing lists and atoms.

; This atom? function is a bit different from what Petite Chez Scheme (which is what I currently use) implements.
; Therefore, it is included here so that other programs calling the function can run correctly.
(define atom?
	(lambda (x)
		(and (not (pair? x)) (not (null? x)))
	)
)

(define add1
	(lambda (n)
		(+ n 1)
	)
)

; Enhanced version of the function rember in Chapter_3.scm.
(define rember*
	(lambda (a L)
		(cond
			((null? L) '())
			((atom? (car L)) ; When (car L) is an atom.
				(cond
					((eq? a (car L)) (rember* a (cdr L)))
					(else (cons (car L) (rember* a (cdr L))))
				)
			)
			; When (car L) is a list.
			(else (cons (rember* a (car L)) (rember* a (cdr L))))
			; The SECOND argument to cons must be a LIST. The result is a list.
		)
	)
)

; Insert new behind old in L.
; Enhanced version of the function insertR in Chapter_3.scm.
(define insertR*
	(lambda (new old L)
		(cond
 			((null? L) '())
			((atom? (car L)) ; When (car L) is an atom.
				(cond
					((eq? old (car L)) (cons old (cons new (insertR* new old (cdr L)))))
					(else (cons (car L) (insertR* new old (cdr L))))
				)
			)
			; When (car L) is a list.
			(else (cons (insertR* new old (car L)) (insertR* new old (cdr L))))
			; Note that the statement recurs with both (car L) and (cdr L).
		)
	)
)

; The function counts the number of times an atom a appears in a list.
; Enhanced version of the function occur in Chapter_4.scm.
(define occur*
	(lambda (a L)
		(cond
			((null? L) 0)
			((atom? (car L))
				(cond
					((eq? a (car L)) (add1 (occur* a (cdr L))))
					(else (occur* a (cdr L)))
				)
			)
			(else (+ (occur* a (car L)) (occur* a (cdr L))))
		)
	)
)

; The function replaces all occurrences of old in a list with new.
; Enhanced version of the function multisubst in Chapter_3.scm.
(define subst*
	(lambda (new old L)
		(cond
			((null? L) '())
			((atom? (car L))
				(cond
					((eq? old (car L)) (cons new (subst* new old (cdr L))))
					(else (cons (car L) (subst* new old (cdr L))))
				)
			)
			(else (cons (subst* new old (car L)) (subst* new old (cdr L))))
		)
	)
)

; The function builds a list with new inserted to the left of all ocurrences of old.
; Enhanced version of the function insertL in Chapter_3.scm.
(define insertL*
	(lambda (new old L)
		(cond
			((null? L) '())
			((atom? (car L))
				(cond
					((eq? old (car L)) (cons new (cons old (insertL* new old (cdr L)))))
					(else (cons (car L) (insertL* new old (cdr L))))
				)
			)
			(else (cons (insertL* new old (car L)) (insertL* new old (cdr L))))
		)
	)
)

; The function checks if L contains a.
; Enhanced version of the function member? in Chapter_2.scm.
(define member*
	(lambda (a L)
		(cond
			((null? L) #f)
			((atom? (car L))
				(or (eq? a (car L)) (member* a (cdr L)))
				; Using keyword or here instead of keyword else makes the piece of codes uniform.
			)
			(else (or (member* a (car L)) (member* a (cdr L))))
		)
	)
)

; The function leftmost finds the leftmost atom in a non-empty list of S-expressions that does not contain the empty list.
(define leftmost
	(lambda (L)
		(cond
			; ((null? L) '())
			((atom? (car L)) (car L))
			(else (leftmost (car L)))
		)
	)
)

; It is the same function from Chapter_4.scm.
; The function returns #t if its two arguments are the same atom.
; All two arguments have to be atoms.
(define eqan?
	(lambda (a1 a2)
		(cond
			((and (number? a1) (number? a2)) (= a1 a2))
			((not (or (number? a1) (number? a2))) (eq? a1 a2))
			; Both of them are not numbers.
			(else #f)
		)
	)
)

; The function using eqan? determines if two lists are equal.
; Verbose version.
(define eqlist?
	(lambda (L1 L2)
		(cond
 			((null? L1)
				(cond
					((null? L2) #t)
					((atom? (car L2)) #f)
					(else #f) ; If (car L2) is a list.
				)
			)
			((atom? (car L1))
				(cond
					((null? L2) #f)
					((atom? (car L2)) (and (eqan? (car L1) (car L2)) (eqlist? (cdr L1) (cdr L2))))
					(else #f)
				)
			)
			(else ; If (car L1) is a list.
				(cond
					((null? L2) #f)
					((atom? (car L2)) #f)
					(else (and (eqlist? (car L1) (car L2)) (eqlist? (cdr L1) (cdr L2))))
				)
			)
		)
	)
)
