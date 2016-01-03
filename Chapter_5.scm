; CHAPTER FIVE
; Author: '(Yongzhen R.)


; Arguments like new, old and a are atoms; L is a list of S-expressions;
; S is an S-expression.

; atom? and add1 are included here so that other programs calling the function can run correctly.
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

; Enhanced version of the function rember in Chapter 3.
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

; Check if a list is a lat.
(define lat?
	(lambda (L)
		(cond
			((null? L) #t)
			((atom? (car L)) (lat? (cdr L)))
			(else #f)
		)
	)
)

; Insert new behind old in L.
; Enhanced version of the function insertR in Chapter 3.
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
; Enhanced version of the function occur in Chapter 4.
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
; Enhanced version of the function multisubst in Chapter 3.
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
; Enhanced version of the function insertL in Chapter 3.
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
; Enhanced version of the function member? in Chapter 2.
(define member*
	(lambda (a L)
		(cond
			((null? L) #f)
			((atom? (car L))
				(cond
					((eq? a (car L)) #t)
					(else (member* a (cdr L)))
				)
				; (or (eq? a (car L)) (member* a (cdr L)))
				; The statement below has the same effect.
			)
			(else (or (member* a (car L)) (member* a (cdr L))))
		)
	)
)

; The function leftmost finds the leftmost atom in a non-empty list of S-expressions
; that does not contain the empty list.
(define leftmost
	(lambda (L)
		(cond
			((atom? (car L)) (car L))
			(else (leftmost (car L)))
		)
	)
)

; (and alpha beta) == (cond (alpha beta) (else #f))
; (or alpha beta) == (cond (alpha #t) (else #f))
; Boolean operators are short-circuit in Scheme.

; It is the same function from Chapter 4.
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
; Textbook version 1.
(define eqlist?
	(lambda (L1 L2)
		(cond
			; When (null? L1) is #t; i.e. L1 is empty.
			((and (null? L1) (null? L2)) #t)
			((and (null? L1) (atom? (car L2))) #f)
			((null? L1) #f)
			; When (atom? (car L1)) is #t; i.e. L1 is an atom.
			((and (atom? (car L1)) (null? L2)) #f)
			((and (atom? (car L1)) (atom? (car L2))) (and (eqan? (car L1) (car L2)) (eqlist? (cdr L1) (cdr L2))))
			; ((and (atom? (car L1)) (atom? (car L2)) (eqan? (car L1) (car L2))) (eqlist? (cdr L1) (cdr L2)))
			; Both of statements above have the same effect.
			((atom? (car L1)) #f)
			; When (car L1) is a list.
			((null? L2) #f)
			((atom? (car L2)) #f)
			(else (and (eqlist? (car L1) (car L2)) (eqlist? (cdr L1) (cdr L2))))
		)
	)
)

; The function using eqan? determines if two lists are equal.
; Improved way of textbook version 1.
(define eqlist?
	(lambda (L1 L2)
		(cond
 			((null? L1)
				(cond
					((null? L2) #t)
					; ((atom? (car L2)) #f) ; The statement can be removed.
					(else #f)
				)
			)
			((atom? (car L1))
				(cond
					((null? L2) #f)
					((atom? (car L2)) (and (eqan? (car L1) (car L2)) (eqlist? (cdr L1) (cdr L2))))
					(else #f)
				)
			)
			(else ; When (car L1) is a list.
				(cond
					((null? L2) #f)
					((atom? (car L2)) #f)
					(else (and (eqlist? (car L1) (car L2)) (eqlist? (cdr L1) (cdr L2))))
				)
			)
		)
	)
)

; The function using eqan? determines if two lists are equal.
; Textbook version 2.
(define eqlist?
	(lambda (L1 L2)
		(cond
			((and (null? L1) (null? L2)) #t)
			((or (null? L1) (null? L2)) #f) ; One of (null? L1) and (null? L2) must be #t here.
			; From the statement below, both of L1 and L2 cannot be null.
			((and (atom? (car L1)) (atom? (car L2))) (and (eqan? (car L1) (car L2)) (eqlist? (cdr L1) (cdr L2))))
			((or (atom? (car L1)) (atom? (car L1))) #f)
			(else (and (eqlist? (car L1) (car L2)) (eqlist? (cdr L1) (cdr L2))))
		)
	)
)

; The function uses eqan? and eqlist? to check if S1 and S2 are the same S-expression.
; eq? / eqan? (atoms) ---> eqlist? (lists) ---> equal? (S-expressions)
(define equal?
	(lambda (S1 S2)
		(cond
			((atom? S1)
				(cond
					((atom? S2) (eqan? S1 S2))
					(else #f)
				)
			)
			(else
				(cond
					((atom? S2) #f)
					(else (eqlist? S1 S2))
				)
			)
		)
	)
)

; A more concise version of function equal?.
(define equal?
	(lambda (S1 S2)
		(cond
			((and (atom? S1) (atom? S2)) (eqan? S1 S2)) ; Both S1 and S2 are atoms.
			((or (atom? S1) (atom? S2)) #f) ; One of S1 and S2 is an atom.
			(else (eqlist? S1 S2)) ; Both S1 and S2 are lists.
		)
	)
)

; Simplified version of function eqlist? using equal?.
; N.B.: now eqlist? and equal? invoke each other in their function body.
(define eqlist?
	(lambda (L1 L2)
		(cond
 			((and (null? L1) (null? L2)) #t)
			((or (null? L1) (null? L2)) #f)
			; If both L1 and L2 are not empty lists.
			((and (equal? (car L1) (car L2)) (eqlist? (cdr L1) (cdr L2))))
		)
	)
)

; Go through a list L and remove the first occurence of S.
; Simplified version of the function rember in Chapter 3.
(define rember
	(lambda (S L)
		(cond
			((null? L) '())
			((equal? S (car L)) (cdr L)) ; Simply replace eq? with equal?.
			(else (cons (car L) (rember S (cdr L))))
		)
	)
)
