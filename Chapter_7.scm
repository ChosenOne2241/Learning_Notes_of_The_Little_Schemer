; CHAPTER SEVEN
; Author: '(Yongzhen R.)


; Here we use member? function.
; set? checks if a lat is a set.
(define set?
	(lambda (lat)
		(cond
			((null? lat) #t)
			((member? (car lat) (cdr lat)) #f)
			(else (set? (cdr lat)))
		)
	)
)

; The function checks if L contains x.
; x has to be an atom here.
; Exactly the same function from Chapter 2, used by makeset function.
(define member?
	(lambda (x lat)
		(cond
			((null? lat) #f)
			((eq? x (car lat)) #t)
			(#t (member? x (cdr lat)))
		)
	)
)

; The function makes a lat a set by removing repetitve elements using member.
(define makeset
	(lambda (lat)
		(cond
			((null? lat) '())
			((member? (car lat) (cdr lat)) (makeset (cdr lat)))
			(else (cons (car lat) (makeset (cdr lat))))
		)
	)
)

; Go through a list and remove the all occurences of atom x.
; x has to be an atom here; L is a lat.
; Exactly the same function from Chapter 3, used by makeset function.
(define multirember
	(lambda (x L)
		(cond
			((null? L) '())
			((eq? x (car L)) (multirember x (cdr L)))
			(#t (cons (car L) (multirember x (cdr L))))
		)
	)
)

; A different implementation of makeset using multirember function.
(define makeset
	(lambda (lat)
		(cond
			((null? lat) '())
			(else (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))
		)
	)
)

; The function checks if set1 is a subset of set2,
; where set1 and set2 are sets.
(define subset?
	(lambda (set1 set2)
		(cond
			((null? set1) #t)
			((member? (car set1) set2) (subset? (cdr set1) set2))
			(else #f)
		)
	)
)

; subset? function with (and ...).
(define subset?
	(lambda (set1 set2)
		(cond
			((null? set1) #t)
			(else (and (member? (car set1) set2) (subset? (cdr set1) set2)))
		)
	)
)

; eqset? checks if two sets are the same.
(define eqset?
	(lambda (set1 set2)
		(and (subset? set1 set2) (subset? set2 set1))
	)
)

; intersect? checks if two sets have intersection.
(define intersect?
	(lambda (set1 set2)
		(cond
			((null? set1) #f)
			((member? (car set1) set2) #t)
			(else (intersect? (cdr set1) set2))
		)
	)
)

; intersect? function with (or ...).
(define intersect?
	(lambda (set1 set2)
		(cond
			((null? set1) #f)
			(else (or (member? (car set1) set2) (intersect? (cdr set1) set2)))
		)
	)
)

; The function returns the intersection of two sets.
(define intersect
	(lambda (set1 set2)
		(cond
			((null? set1) '())
			((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
			(else (intersect (cdr set1) set2))
		)
	)
)

; The function returns the union of two sets.
(define union
	(lambda (set1 set2)
		(cond
			((null? set1) set2)
			((member? (car set1) set2) (union (cdr set1) set2))
			(else (cons (car set1) (union (cdr set1) set2)))
		)
	)
)

; The function returns the difference of two sets (set1 / set2)
(define difference
	(lambda (set1 set2)
		(cond
			((null? set1) '())
			((member? (car set1) set2) (difference (cdr set1) set2))
			(else (cons (car set1) (difference (cdr set1) set2)))
		)
	)
)
