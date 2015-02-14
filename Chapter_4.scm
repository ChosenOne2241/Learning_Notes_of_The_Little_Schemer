;; CHAPTER FOUR
;; Author: Yongzhen Ren
;; In this chapter, we assume that all inputs should be integers.

;; add1 and sub1 are built-in functions in Petite.
(define add1
	(lambda (n)
		(+ n 1)
	)
)

(define sub1
	(lambda (n)
		(- n 1)
	)
)

;; Addition.
;; o+ is to number as cons is to list.
;; Warning: a has to be bigger than or equal to 0.
;; e.g: a + b
;;      4 + 14
;;      3 + 15
;;      2 + 16
;;      1 + 17
;;      0 + 18 ---> return b as the final result
(define o+
	(lambda (a b)
		(cond
			((zero? a) b)
			(else (o+ (sub1 a) (add1 b)))
			;; (else (sub1 (o+ a (add1 b))))
			;; The above statement has the same effect.
		)
	)
)

;; Subtraction.
;; Warning: b has to be bigger than or equal to 0.
(define o-
	(lambda (a b)
		(cond
			((zero? b) a)
			(else (o- (sub1 a) (sub1 b)))
			;; (else (sub1 (o- a (sub1 b))))
			;; The above statement has the same effect.
		)
	)
)

;; The function builds a number by totaling all the numbers in a tup.
;; tup is short for tuple, which is a list of numbers.
(define addup
	(lambda (tup)
		(cond
			((null? tup) 0)
			(#t (+ (car tup) (addup (cdr tup))))
		)
	)
)

;; Multiplication.
;; Warning: n has to be bigger than or equal to 0.
(define x
	(lambda (n m)
		(cond
			((zero? n) 0)
			(#t (+ m (x (sub1 n) m)))
		)
	)
)

;; The function traverses two tups at the same time and add two numbers with the same index to return a new tup.
;; tup1 and tup2 can be different length.
(define tup+
	(lambda (tup1 tup2)
		(cond
			((null? tup1) tup2)
			((null? tup2) tup1)
			(#t (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))
		)
	)
)

;; Both n and m have to be bigger than or equal to 0.
(define >
	(lambda (n m)
		(cond
			((zero? n) #f)
			((zero? m) #t)
			;; The order of the two terminal conditions matters.
			(#t (> (sub1 n) (sub1 m)))
		)
	)
)

;; n and m have to be bigger than or equal to 0.
(define <
	(lambda (n m)
		(cond
			((zero? m) #f)
			((zero? n) #t)
			;; The order of the two terminal conditions matters.
			(#t (< (sub1 n) (sub1 m)))
		)
	)
)

;; Both n and m have to be bigger than or equal to 0.
;; The code is based on such an idea:
;; 1. if (n == 0) and (m == 0) ---> #t
;; 2. if (n != 0) and (m != 0) ---> Recursion.
;; 3. if (n == 0) and (m != 0) ---> #f
;; 4. if (n != 0) and (m == 0) ---> #f
(define =
	(lambda (n m)
		(cond
			((and (zero? n) (zero? m)) #t) ;; 1
			((not (or (zero? n) (zero? m))) (= (sub1 n) (sub1 m))) ;; 2
			(else #f) ;; 3 & 4
		)
	)
)

;; Different implementation of = function.
(define =
	(lambda (n m)
		(cond
			((zero? n) (zero? m)) ;; 1 & 3
			((zero? m) #f) ;; 4
			(else (= (sub1 n) (sub1 m)))
		)
	)
)

;; Another implementation of = function using > and <. (Non-recursion)
(define =
	(lambda (n m)
		(cond
			((> n m) #f)
			((< n m) #f)
			(else #t)
		)
	)
)

;; Calculate n^m.
;; m has to be bigger than or equal to 0.
(define expt
	(lambda (n m)
		(cond
			((zero? m) 1)
			(else (* n (expt n (sub1 m))))
		)
	)
)

;; Division.
;; m has to be bigger than or equal to 0.
(define div
	(lambda (n m)
		(cond
			((< n m) 0)
			(else (add1 (div (- n m) m)))
		)
	)
)

;; Return the number of S-expressions in a lat.
(define length
	(lambda (lat)
		(cond
			((null? lat) 0)
			(else (add1 (length (cdr lat))))
		)
	)
)

;; Pick the nth S-expression from a lat.
;; n should be bigger than 0; lat cannot be an empty list and the length of lat has to be bigger than 0.
(define pick
	(lambda (n lat)
		(cond
			((zero? (sub1 n)) (car lat))
			(#t (pick (sub1 n) (cdr lat)))
		)
	)
)

;; Remove the nth S-expression in a lat.
;; n should be bigger than 0; lat cannot be an empty list and the length of lat has to be bigger than 0.
(define rempick
	(lambda (n lat)
		(cond
			((zero? (sub1 n)) (cdr lat))
			(#t (cons (car lat) (rempick (sub1 n) (cdr lat))))
		)
	)
)

;; Remove all numbers from a lat.
(define no-nums
	(lambda (lat)
		(cond
			((null? lat) '())
			((number? (car lat)) (no-nums (cdr lat)))
			(#t (cons (car lat) (no-nums (cdr lat))))
		)
	)
)

;; The function extracts a tup from a lat using all the numbers in the lat.
(define all-nums
	(lambda (lat)
		(cond
			((null? lat) '())
			((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
			(#t (all-nums (cdr lat)))
		)
	)
)

;; The function returns #t if its two arguments are the same atom.
;; Warning: all two arguments have to be atoms.
(define eqan?
	(lambda (a1 a2)
		(cond
			((and (number? a1) (number? a2)) (= a1 a2))
			((not (or (number? a1) (number? a2))) (eq? a1 a2))
			;; Both of them are not numbers.
			(else #f)
		)
	)
)

;; The function counts the number of times an atom a appears in a lat.
;; Warning: a has to be an atom.
(define occur
	(lambda (a lat)
		(cond
			((null? lat) 0)
			((eq? a (car lat)) (add1 (occur a (cdr lat))))
			(#t (occur a (cdr lat)))
		)
	)
)

;; The function returns #t if n is 1 and #f otherwise.
;; n should be a number.
(define one?
	(lambda (n)
		(cond
			((zero? (sub n)) #t)
			(else #f)
		)
	)
)

;; A more concise version of function one?.
(define one?
	(lambda (n)
		(= n 1)
	)
)

;; Rewritten version of function rempick using function one?.
(define rempick
	(lambda (n lat)
		(cond
			((one? n) (cdr lat))
			(#t (cons (car lat) (rempick (sub1 n) (cdr lat))))
		)
	)
)
