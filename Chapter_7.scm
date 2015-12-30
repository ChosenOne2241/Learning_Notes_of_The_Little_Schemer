; CHAPTER SEVEN
; Author: '(Yongzhen R.)


; The function checks if L contains x.
; x has to be an atom here.
; Exactly the same function from Chapter 2, used by set? and makeset function.
(define member?
	(lambda (x lat)
		(cond
			((null? lat) #f)
			((eq? x (car lat)) #t)
			(#t (member? x (cdr lat)))
		)
	)
)

; Here we use member? function.
; set? checks if a lat is a set of atoms.
(define set?
	(lambda (lat)
		(cond
			((null? lat) #t)
			((member? (car lat) (cdr lat)) #f)
			(else (set? (cdr lat)))
		)
	)
)

; The function turns a lat into a set by removing repetitve elements.
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

; A different implementation of makeset using multirember function
; according to previous one.
(define makeset
	(lambda (lat)
		(cond
			((null? lat) '())
			((member? (car lat) (cdr lat)) (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))
			(else (cons (car lat) (makeset (cdr lat))))
		)
	)
)

; A different implementation of makeset using multirember function from the textbook.
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

; The function returns the difference of two sets (set1 / set2).
(define difference
	(lambda (set1 set2)
		(cond
			((null? set1) '())
			((member? (car set1) set2) (difference (cdr set1) set2))
			(else (cons (car set1) (difference (cdr set1) set2)))
		)
	)
)

; The function takes a set of lat as argument and return the intersection of all lats.
(define intersectall
	(lambda (l-set)
		(cond
			((null? (cdr l-set)) (car l-set))
			(else (intersect (car l-set) (intersectall (cdr l-set)))) 
		)
	)
)

; Exactly the same function from Chapter 2, used in a-pair?.
(define atom?
	(lambda (x)
		(and (not (pair? x)) (not (null? x)))
	)
)

; Pair is a list containing exactly two S-expressions, such as (a (a b)), (3 4).
; a-pair? checks if L is a pair.
(define a-pair?
	(lambda (L)
		(cond
			; An S-expression.
			((atom? L) #f) ; If it is an atom.
			; A list.
			((null? L) #f) ; If it is empty.
			; A non-empty list with one or two or more elements.
			((null? (cdr L)) #f) ; If it contains only one element.
			; A non-empty list with two or more elements.
			((null? (cdr (cdr L))) #t) ; ((null? (cddr L)) #t)
			; If the third element is null (it contains exactly two elements).
			(else #f) ; A non-empty list with more than two elements.
		)
	)
)

; first gets the first element from a list.
(define first
	(lambda (L)
		(car L)
	)
)

; second gets the second element from a list.
(define second
	(lambda (L)
		(car (cdr L)) ; (cadr L)
	)
)

; third gets the third element from a list.
(define third
	(lambda (L)
		(car (cdr (cdr L))) ; (caddr L)
	)
)

; The function builds a pair with two S-expressions.
(define build
	(lambda (s1 s2)
		(cons s1 (cons s2 '())) ; Don't forget to cons '().
		; (cons x1 (cons x2 '())) == (list x1 x2)
		; where x1 and x2 are S-expressions.
	)
)

; Return all the first ones in order of a list of non-empty lists or return a null list.
; Exactly the same function from Chapter 3, used in fun?.
(define firsts
	(lambda (L)
		(cond
			((null? L) '())
			; Since (car L) is non-empty, (car (car L)) cannot be empty too.
			(else (cons (car (car L)) (firsts (cdr L))))
			; (else (cons (caar L) (firsts (cdr L))))
		)
	)
)

; The term "relation" (rel) means a set of pairs, such as ((1 2) (1 3) (3 4)).
; The term "function" (fun) means a set of pairs, where the fact that (firsts X_i)
; is a set holds, for each element X_i in the set, such as ((1 2) (2 3) (3 3)).
; fun? is to check if a rel is a fun.
(define fun?
	(lambda (rel)
		(set? (firsts rel))
	)
)

; revrel switches the order of elements in each pair.
(define revrel
	(lambda (rel)
		(cond
			((null? rel) '())
			(else (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel))))
		)
	)
)

; revpair reverses the two components of a pair,
; used in revised revrel function.
(define revpair
	(lambda (pair)
		(build (second pair) (first pair))
	)
)

; Rewrite the revrel function using revpair.
(define revrel
	(lambda (rel)
		(cond
			((null? rel) '())
			(else (cons (revpair (car rel)) (revrel (cdr rel))))
		)
	)
)

; The function returns all the second ones in order of a list of lists with at least two elements
; or returns a null list.
(define seconds
	(lambda (L)
		(cond
			((null? L) '())
			((null? (cdr (car L))) (seconds (cdr L)))
			; Although (car (car L)) cannot be empty,
			; it does not mean (cdr (car L)) has to return non-empty element.
			(else (cons (car (cdr (car L))) (seconds (cdr L))))
		)
	)
)

; The term "full function" (fullfun) means a fun with all second items being a set.
; fullfun? is to check if a rel is a full function.
(define fullfun?
	(lambda (fun)
		(set? (seconds fun))
	)
)

; one-to-one? is an alias of fullfun?.
(define one-to-one?
	(lambda (fun)
		(fun? (revrel fun))
		; For one-to-one function, it exists its inverse function.
	)
)
