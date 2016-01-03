; CHAPTER SIX
; Author: '(Yongzhen R.)


(define atom?
	(lambda (x)
		(and (not (pair? x)) (not (null? x)))
	)
)

; Calculate x ^ y, where x cannot be 0 and y is a positive integer.
; The almost same implementation of expt function in Chapter 4.
(define ^
	(lambda (x y)
		(cond
			((= y 0) 1)
			(else (* x (^ x (- y 1))))
		)
	)
)

; (eq? 'a 'a) => #t
; (define x 'a)
; (define y 'a)
; (eq? x y) => #t (in the textbook) / #f (in Petite)

; numbered? determines whether a representation of an arithmetic expression
; contains only numbers and opeartors (+, * and ^).
; For the purpose of this chapter, an arithmetic expression is either an atom
; (including numbers), or two arithmetic expressions combined by +, *, or ^
; (infix expression), which means the program can only handle expressions like
; ((3 + 6) * (4 ^ 2)) (where same structures are at two sides of the operand)
; instead of (3 + 4 + 5).
(define numbered?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			((eq? (car (cdr aexp)) '+) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) '*) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) '^) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
		)
	)
)

; Since aexp was already understood to be an arithmetic expression,
; there is a simplified version of numbered?.
(define numbered?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			(else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
		)
	)
)

; value returns the value of a regular arithmetic expression with numbers
; and opeartors (+, * and ^).
(define value
	(lambda (nexp)
		(cond
			((atom? nexp) nexp)
			((eq? (car (cdr nexp)) '+) (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
			((eq? (car (cdr nexp)) '*) (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
			((eq? (car (cdr nexp)) '^) (^ (value (car nexp)) (value (car (cdr (cdr nexp))))))
		)
	)
)

; Help function 1.
(define 1st-sub-exp
	(lambda (aexp)
		(car (cdr aexp))
	)
)

; Help function 2.
(define 2nd-sub-exp
	(lambda (aexp)
		(car (cdr (cdr aexp)))
	)
)

; Help function 3.
(define operator
	(lambda (aexp)
		(car aexp)
	)
)

; Using three help functions (1st-sub-exp, 2nd-sub-exp and operator),
; rewrite value function to parse Polish expression (such as (+ 3 4)):
; - number,
; - a list of the atom + followed by two arithmetic expressions,
; - a list of the atom * followed by two arithmetic expressions, or
; - a list of the atom ^ followed by two arithmetic expressions.
(define value
	(lambda (nexp)
		(cond
			((atom? nexp) nexp)
			((eq? (operator nexp) '+) (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
			((eq? (operator nexp) '*) (* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
			((eq? (operator nexp) '^) (^ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
		)
	)
)

; Use parentheses to represent natual numbers:
; () - 0, (()) - 1, (()()) - 2 etc.
; sero? is to check if it is ().
(define sero?
	(lambda (n)
		(null? n)
	)
)

; Similar to add1 function for natural numbers.
(define edd1
	(lambda (n)
		(cons '() n)
	)
)

; Similar to sub1 function for natural numbers.
(define zub1
	(lambda (n)
		(cdr n)
	)
)

; Similar to addition for natural numbers.
(define edd
	(lambda (m n)
		(cond
			((sero? m) n)
			(else (edd (zub1 m) (edd1 n)))
			; (else (edd1 (edd m (zub1 n))))
		)
	)
)

; A little bit neater implementation of lat? function from Chapter 2.
(define lat?
	(lambda (L)
		(cond
			((null? L) #t)
			((atom? (car L)) (lat? (cdr L)))
			(else #f)
		)
	)
)
