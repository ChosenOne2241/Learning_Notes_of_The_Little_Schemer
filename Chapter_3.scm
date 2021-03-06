; CHAPTER THREE
; Author: '(Yongzhen R.)


; Go through a lat and remove the first occurence of atom x.
(define rember
	(lambda (x lat)
		(cond
			((null? lat) '())
			((eq? x (car lat)) (cdr lat))
			(#t (cons (car lat) (rember x (cdr lat))))
		)
	)
)

; Return all the first ones in order from a list of non-empty lists or return an empty list.
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

; Both arguments new and old have to be atoms.
; The function builds a lat with new inserted to the right of the first ocurrence of old.
(define insertR
	(lambda (new old lat)
		(cond
			((null? lat) '()) ; '() is equal to (quote ()) in Scheme.
			((eq? old (car lat)) (cons old (cons new (cdr lat))))
			; Connect the first atom (currently), the new one and the rest of lat together.
			(#t (cons (car lat) (insertR new old (cdr lat))))
		)
	)
)

; The function builds a lat with new inserted to the left of the first ocurrence of old.
(define insertL
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? old (car lat)) (cons new lat))
			; ((eq? old (car lat)) (cons new (cons old (cdr lat))))
			; The above statement has the same effect.
			(#t (cons (car lat) (insertL new old (cdr lat))))
		)
	)
)

; The function replaces the first occurrence of old in the lat with new.
(define subst
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? old (car lat)) (cons new (cdr lat)))
			(#t (cons (car lat) (subst new old (cdr lat))))
		)
	)
)

; The function replaces either the first occurrence of o1 or the first occurrence of o2 by new.
(define subst2
	(lambda (new o1 o2 lat)
		(cond
			((null? lat) '())
			((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
			(#t (cons (car lat) (subst2 new o1 o2 (cdr lat))))
		)
	)
)

; Go through a list and remove the all occurences of atom x.
; x has to be an atom here; L is a lat.
(define multirember
	(lambda (x L)
		(cond
			((null? L) '())
			((eq? x (car L)) (multirember x (cdr L)))
			(#t (cons (car L) (multirember x (cdr L))))
		)
	)
)

; Both arguments new and old need to be atoms.
; The function builds a lat with new inserted to the right of the all ocurrences of old.
(define multiinsertR
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
			(#t (cons (car lat) (multiinsertR new old (cdr lat))))
		)
	)
)

; Both arguments new and old need to be atoms.
; The function builds a lat with new inserted to the left of the all ocurrences of old.
(define multiinsertL
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
			(else (cons (car lat) (multiinsertL new old (cdr lat))))
		)
	)
)

; The function replaces the all occurrences of old in a lat with new.
(define multisubst
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
			(#t (cons (car lat) (multisubst new old (cdr lat))))
		)
	)
)
