; CHAPTER EIGHT
; Author: '(Yongzhen R.)


(define atom?
	(lambda (x)
		(and (not (pair? x)) (not (null? x)))
	)
)

; rember-f goes through l and remove the first occurence of a according to
; testing condition test?. General version of the same function in Chapter 3.
(define rember-f
	(lambda (test? a l)
		(cond
			((null? l) '())
			((test? a (car l)) (cdr l))
			(else (cons (car l) (rember-f test? a (cdr l))))
		)
	)
)

; eq?-c is a function that, when passed an argument a, returns the function
; (lambda (x)
; 	(eq? x a))
; where a is just that argument.
(define eq?-c
	(lambda (a)
		(lambda (x) ; Currying.
			(eq? x a)
		)
	)
)

; Rewrite rember-f as a function of one argument test? that returns an argument
; like rember with eq? replaced by test?.
(define rember-f
	(lambda (test?)
		(lambda (a l)
			(cond
				((null? l) '())
				((test? a (car l)) (cdr l))
				(else (cons (car l) ((rember-f test?) a (cdr l))))
			)
		)
	)
)

; The function builds a list with new inserted to the left of the first ocurrence
; of old according to testing condition test?.
(define insertL-f
	(lambda (test?)
		(lambda (new old l)
			(cond
				((null? l) '())
				((test? old (car l)) (cons new (cons old (cdr l))))
				(else (cons (car l) ((insertL-f test?) new old (cdr l))))
			)
		)
	)
)

; The function builds a list with new inserted to the right of the first
; ocurrence of old according to testing condition test?.
(define insertR-f
	(lambda (test?)
		(lambda (new old l)
			(cond
				((null? l) '())
				((test? old (car l)) (cons old (cons new (cdr l))))
				(else (cons (car l) ((insertR-f test?) new old (cdr l))))
			)
		)
	)
)

; seqL conses the first argument onto the result of consing the second onto the third.
(define seqL
	(lambda (new old l)
		(cons new (cons old l))
	)
)

; seqR conses the second argument onto the result of consing the first onto the third.
(define seqR
	(lambda (new old l)
		(cons old (cons new l))
	)
)

; General version of insert function using help functions (seqL and seqR).
(define insert-g
	(lambda (seq)
		(lambda (new old l)
			(cond
				((null? l) '())
				((eq? old (car l)) (seq new old (cdr l)))
				(else (cons (car l) ((insert-g seq) new old (cdr l))))
			)
		)
	)
)

; Define insertL with insert-g.
(define insertL (insert-g seqL))

; Define insertR with insert-g.
(define insertR (insert-g seqR))

; Define insertL without using seqL.
(define insertL
	(insert-g
		(lambda (new old l)
			(cons new (cons old l))
		)
	)
)

; Define insertR without using seqR.
(define insertR
	(insert-g
		(lambda (new old l)
			(cons old (cons new l))
		)
	)
)

(define seqS
	(lambda (new old l)
		(cons new l)
	)
)

; Define subst (appearing in Chapter 3) using insert-g.
(define subst (insert-g seqS))

(define seqrem
	(lambda (new old l)
		l
	)
)

; Define rember using insert-g.
(define rember
	(lambda (a l)
		((insert-g seqrem) #f a l)
		; #f here is just a spacefiller.
	)
)

; Calculate x ^ y, where x cannot be 0 and y is a positive integer.
; Exactly the same function from Chapter 6, used in value function.
(define ^
	(lambda (x y)
		(cond
			((= y 0) 1)
			(else (* x (^ x (- y 1))))
		)
	)
)

; Help function 1 from Chapter 6.
(define 1st-sub-exp
	(lambda (aexp)
		(car (cdr aexp))
	)
)

; Help function 2 from Chapter 6.
(define 2nd-sub-exp
	(lambda (aexp)
		(car (cdr (cdr aexp)))
	)
)

; Help function 3 from Chapter 6.
(define operator
	(lambda (aexp)
		(car aexp)
	)
)

(define atom-to-function
	(lambda (x)
		(cond
			((eq? x '+) +)
			((eq? x '*) *)
			((eq? x '^) ^)
		)
	)
)

; Rewrite value function from Chapter 6.
(define value
	(lambda (nexp)
		(cond
			((atom? nexp) nexp)
			(else ((atom-to-function (operator nexp)) (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
		)
	)
)

; Go through a list and remove the all occurences of atom x
; according to testing condition test?.
(define multirember-f
	(lambda (test?)
		(lambda (a lat)
			(cond
				((null? lat) '())
				((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
				(else (cons (car lat) ((multirember-f test?) a (cdr lat))))
			)
		)
	)
)

; Define multirember-eq? using multirember-f.
(define multirember-eq? (multirember-f eq?))

; It looks at every atom of the lat to see whether it is eq? to a.
; Those atoms that are not are collected in one list ls1; the others for which
; the answer is true are collected in a second list ls2.
; Finally, it determines the value of (f ls1 ls2).
(define multirember&co
	(lambda (a lat col)
		(cond
			((null? lat) (col '() '()))
			((eq? (car lat) a)
				(multirember&co a (cdr lat)
					(lambda (newlat seen)
						(col newlat (cons (car lat) seen))
					)
				)
			)
			(else
				(multirember&co a (cdr lat)
					(lambda (newlat seen)
						(col (cons (car lat) newlat) seen)
					)
				)
			)
		)
	)
)

; It is a function that takes two arguments and asks whether the second
; one is the empty list. It ignores its first argnment.
; It is used to test multirember&co.
(define a-friend
	(lambda (x y)
		(null? y)
	)
)

; Both arguments new and old need to be atoms.
; The function builds a lat with new inserted to the left of the all ocurrences of old.
; Similar to its counterpart in Chapter 3 (multiinsertR).
(define multiinsertL
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
			(else (cons (car lat) (multiinsertL new old (cdr lat))))
		)
	)
)

; A general version of multiinsert fucntion using seq as an argument (seqL / seqR).
(define multiinsert
	(lambda (seq)
		(lambda (new old lat)
			(cond
				((null? lat) '())
				((eq? old (car lat)) (seq new old ((multiinsert seq) new old (cdr lat))))
				(else (cons (car lat) ((multiinsert seq) new old (cdr lat))))
			)
		)
	)
)

; multiinsertLR inserts new to the left of oldL and to the right of oldR in lat
; if oldL and oldR are different.
(define multiinsertLR
	(lambda (new oldL oldR lat)
		(cond
			((null? lat) '())
			((eq? oldL (car lat)) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
			((eq? oldR (car lat)) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
			(else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat))))
		)
	)
)

; Exactly the same function from Chapter 4.
(define add1
	(lambda (n)
		(+ n 1)
	)
)

; When multiinsertLR&co is done, it will use col on the new lat ,
; on the number of left insertions, and the number of right insertions.
(define multiinsertLR&co
	(lambda (new oldL oldR lat col)
		(cond
			((null? lat) (col '() 0 0))
			((eq? oldL (car lat))
				(multiinsertLR&co new oldL oldR (cdr lat)
					(lambda (newlat left right)
						(col (cons new (cons oldL newlat)) (add1 left) right)
					)
				)
			)
			((eq? oldR (car lat))
				(multiinsertLR&co new oldL oldR (cdr lat)
					(lambda (newlat left right)
						(col (cons oldR (cons new newlat)) left (add1 right))
					)
				)
			)
			(else (multiinsertLR&co new oldL oldR (cdr lat) col))
		)
	)
)

; check-even? is to check if n is even.
; This function does not work in Petite since it will not truncate while doing division.
(define check-even?
	(lambda (n)
		(= (* (/ n 2) 2) n)
	)
)

; The function removes all odd numbers from a list of nested lists.
; L should just contain numbers or lists of numbers.
(define evens-only*
	(lambda (L)
		(cond
			((null? L) '())
			((atom? (car L))
				(cond
					((even? (car L)) (cons (car L) (evens-only* (cdr L))))
					(else (evens-only* (cdr L)))
				)
			)
			(else (cons (evens-only* (car L)) (evens-only* (cdr L))))
		)
	)
)

; The version of evens-only* using list? to simplify it.
(define evens-only*
	(lambda (L)
		(cond
			; If L is empty.
			((null? L) '())
			; If the first element in L is a list.
			((list? (car L)) (cons (evens-only* (car L)) (evens-only* (cdr L))))
			; If it is even (an atom).
			((even? (car L)) (cons (car L) (evens-only* (cdr L))))
			; If it is odd (an atom).
			(else (evens-only* (cdr L)))
		)
	)
)

; The function builds a nested list of even numbers by removing the odd ones
; from its argument and simultaneously multiplies the even numbers and
; sums up the odd numbers that occur in its argument.
(define evens-only*&co
	(lambda (L col)
		(cond
			((null? L) (col '() 1 0))
			((atom? (car L))
				(cond
					((even? (car L))
						(evens-only*&co (cdr L)
							(lambda (newlist e_pro o_sum)
								(col (cons (car L) newlist) (* (car L) e_pro) o_sum)
							)
						)
					)
					(else
						(evens-only*&co (cdr L)
							(lambda (newlist e_pro o_sum)
								(col newlist e_pro (+ (car L) o_sum))
							)
						)
					)
				)
			)
			(else
				(evens-only*&co (car L) ; (1)
					; The collector conses together the results for the lists in
					; the car and the cdr and multiplies and adds the respective
					; products and sums.
					; Then it passes these values to the old collector.
					; MOST TRICKY PART in the function.
					(lambda (newlist e_pro o_sum)
						(evens-only*&co (cdr L) ; (2)
						; Statements (1) and (2) can exchange their position.
							(lambda (nl ep os)
								(col (cons newlist nl) (* e_pro ep) (+ o_sum os))
							)
						)
					)
				)
			)
		)
	)
)

; The function is used to test evens-only*&co.
; Ask: (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
; Answer: (38 1920 (2 8) 10 (() 6) 2)
(define the-last-friend
	(lambda (newl product sum)
		(cons sum (cons product newl))
	)
)
