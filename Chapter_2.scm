; CHAPTER TWO
; Author: '(Yongzhen R.)


; cons: construct;
; car: contents of address part of register;
; cdr: contents of decrement part of register.

; This atom? function is slightly different from what Petite Chez Scheme
; (which is what I currently use) implements.
; Here is an article about how atom? act in Petite:
; http://www.scheme.com/csug8/objects.html
; Therefore, it is included here so that other programs calling the function can run correctly.
(define atom?
	(lambda (x)
		(and (not (pair? x)) (not (null? x)))
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
