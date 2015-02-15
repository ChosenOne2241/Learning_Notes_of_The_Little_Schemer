; CHAPTER TWO
; Author: Yongzhen Ren


; This atom? function is a bit different from what Petite Chez Scheme (which is what I currently use) implements.
; Therefore, it is included here so that other programs calling the function can run correctly.
(define atom?
	(lambda (x)
		(and (not (pair? x)) (not (null? x)))
	)
)

; Implement built-in function.
; Append function is used to connect two lists.
(define append
	(lambda (A B)
		(cond
			((null? A) B)
			(#t (cons (car A) (append (cdr A) B)))
		)
		; (if (null? A) B (cons (car A) (append (cdr A) B)))
		; The above statement is more concise.
	)
)

; A lat means a list of atoms.
; If the list comprises ALL atoms, the function will return #t. Otherwise, #f.
; Special case: (lat? '()) ---> #t, since it does not contain a list.
(define lat?
	(lambda (L)
		(cond
			((null? L) #t)
			((not (atom? (car L))) #f)
			(#t (lat? (cdr L)))
		)
	)
)

; Implement built-in function.
; The function checks if L contains x.
; x has to be an atom here.
(define member?
	(lambda (x lat)
		(cond
			((null? lat) #f)
			((eq? x (car lat)) #t)
			(#t (member? x (cdr lat)))
			; The keyword *else* can replace #t here, since *else* is always true. 
		)
	)
)
