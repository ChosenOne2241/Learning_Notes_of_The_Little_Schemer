# Glossary

## Tree of Basic Concepts on S-expression

* S-expression
  * Atom
        * Symbol (such as 'a, 'HELLO, and 'F-16)
        * Number (such as 42, 0, and -4.23)
  * List **(Terms below are NOT necessarily mutually exclusive.)**
        * Null List '()
        * Pair
        * Lat
        * Tup
        * Set
             * Rel
             * Fun
             * ...
        * ...

## Explanation of Some Non-trivial Terms
>> According to the order of appearance in the *The Little Schemer*.

* **_S-expression:_**
	symbolic expression; all atoms and lists are S-expression.

* **_Atom:_**
	a string of characters, or a string of digits, or with other special characters.

* **_List:_**
	n (n >= 0) S-expressions enclosed by parentheses.

* **_Lat:_**
	a list of atoms, such as '(1 2 3), '(a), and '(a b 1 2).

* **_Tup (Tuple):_**
	a list of numbers, such as '(1 2 3), '(1 2), and '(3).

* **_Pair:_**
	a list containing exactly two S-expressions,
	such as '(1 2), '((a b) c), and '((ab 1) (2 cd)).

* **_Rel (Relation):_**
	a set of pairs, such as '((1 2) (1 3) (3 4)), and '((a b) (1 c)).

* **_Fun (Function):_**
	a set of pairs, where the return value of (firsts X<sub>i</sub>)
	is a set, for each element X<sub>i</sub> in the set, such as '((1 2) (2 3) (3 3)).

* **_Fullfun (Full Function / One-to-one):_**
	a fun with all second items being a set, such as '((1 2) (3 4) (5 6)).
