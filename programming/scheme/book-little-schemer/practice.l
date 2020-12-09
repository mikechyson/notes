#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

       
;; practice

;; list of atoms
;; check whether the list consists of atoms
;; return true if it does
(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))


;; ;; check whether atom a is a member of lat
;; ;; if it does, return true
;; (define member?
;;   (lambda (a lat)
;;     (cond
;;      ((null? lat) #f)
;;      (else
;;       (or (eq? (car lat) a)
;; 	  (member? a (cdr lat)))))))


;; check whether atom a is a member of lat
;; if it does, return true
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else
      (or (equal? (car lat) a) 		;defined later
	  (member? a (cdr lat)))))))


;; ;; rember: remove a mumber
;; ;; check each atom of the lat, one at a time, to see if it was the same as the atom and.
;; ;; If the car was not the same as the stom, we saved it to the consed to the final value later.
;; ;; When rember found the atom and, it dtropped it,
;; ;; and consed the previous atoms back onto the rest of the lat.
;; (define rember
;;   (lambda (a lat)
;;     (cond
;;      ((null? lat) (quote ()))
;;      ((eq? (car lat) a) (cdr lat))
;;      (else
;;       (cons (car lat)
;; 	    (rember a (cdr lat)))))))

;; takes one argument, a list, which is either a null list or contains only non-empty lists.
;; It builds another list composed of the first S-expression of each internal list.
(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else
      (cons (car (car l))
	    (firsts (cdr l)))))))

;; ;; insert the new atom into the right side of the old atom
;; (define insertR
;;   (lambda (new old lat)
;;     (cond
;;      ((null? lat) (quote ()))
;;      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
;;      (else
;;       (cons (car lat) (insertR new old (cdr lat)))))))

;; ;; insert the new atom into the left side of the old atom
;; (define insertL
;;   (lambda (new old lat)
;;     (cond
;;      ((null? lat) (quote ()))
;;      ((eq? (car lat) old) (cons new lat))
;;      (else
;;       (cons (car lat) (insertL new old (cdr lat)))))))

;; ;; replaces the first occurrence of old in the lat with new
;; (define subst
;;   (lambda (new old lat)
;;     (cond
;;      ((null? lat) (quote ()))
;;      ((eq? (car lat) old) (cons new (cdr lat)))
;;      (else
;;       (cons (car lat) (subst new old (cdr lat)))))))

;; replaces either the first occurrence of o1 or the first occurrence of o2 by new
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote ()))
     ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
     (else
      (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

;; ;; remove all occurrence of a
;; (define multirember
;;   (lambda (a lat)
;;     (cond
;;      ((null? lat) (quote ()))
;;      ((eq? (car lat) a) (multirember a (cdr lat)))
;;      (else
;;       (cons (car lat) (multirember a (cdr lat)))))))



;; remove all occurrence of a
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((equal? (car lat) a) (multirember a (cdr lat)))
     (else
      (cons (car lat) (multirember a (cdr lat)))))))





(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else
      (cons (car lat) (multiinsertR new old (cdr lat)))))))


(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
     (else
      (cons (car lat) (multisubst new old (cdr lat)))))))

;; + is used so i use o+
(define o+
  (lambda (n m)
    (cond
    ((zero? m) n)
    (else
     (add1 (o+ n (sub1 m)))))))

;; - is used so i use o-
(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (sub1 (o- n (sub1 m)))))))

;; i define a function to test tup
(define tup?
  (lambda (tup)
    (cond
     ((null? tup) #t)
     ((and (number? (car tup)) (tup? (cdr tup))) #t)
     (else
      #f))))


;; builds a number by totalling all the numbers in its argument
(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (o+ (car tup) (addtup (cdr tup)))))))

;; because x is used i use ox instead
(define ox
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (o+ n (ox n (sub1 m)))))))
;; (x 12 3)
;; = 12 + (x 12 2)
;; = 12 + 12 (x 12 1)
;; = 12 + 12 + 12 (x 12 0)
;; = 12 + 12 + 12 + 0


;; ;; adds the corresponding numbers in two tups, building a tup of the same length
;; (define tup+
;;   (lambda (tup1 tup2)
;;     (cond
;;      ;; becuase the tups must have the same length, two questions are enough
;;      ((and (null? tup1) (null? tup2) (quote ())))
;;      (else
;;       (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))


;; ;; add two tups up to form a new tup
;; (define tup+
;;   (lambda (tup1 tup2)
;;     (cond
;;      ((and (null? tup1) (null? tup2)) (quote ()))
;;      ((null? tup1) tup2)
;;      ((null? tup2) tup1)
;;      (else
;;       (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))


;; simplified
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

;; becuase > is used
(define o>
  (lambda (n m)
    (cond
     ;; we only consider non-negative numbers
     ;; the order matters, because of the "="
     ((zero? n) #f)
     ((zero? m) #t)
     (else
      (o> (sub1 n) (sub1 m))))))

;; because < is used
(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else
      (o< (sub1 n) (sub1 m))))))

;; ;; because = is used
;; (define o=
;;   (lambda (n m)
;;     (cond
;;      ((zero? n) (zero? m))
;;      ((zero? m) #f)
;;      (else
;;       (o= (sub1 n) (sub1 m))))))



(define o=
  (lambda (n m)
    (cond
     ((> n m) #f)
     ((< n m) #f)
     (else
      #t))))

;; for uniform i use o^ instead of ^
(define o^
  (lambda (n m)
    (cond
     ((zero? m)  1)
     (else
      (ox n (o^ n (sub1 m)))))))


;; / is used so i use o/
(define o/
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else
      (add1 (o/ (o- n m) m))))))

;; (/ 15 4)
;; = 1 + (/ 11 4)
;; = 1 + (1 + (/ 7 4))
;; = 1 + (1 + (1 + (/ 3 5)))
;; = 1 + (1 + (1 + 0))


(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else
      (add1 (length (cdr lat)))))))


;; recurrsion thoutht, notice it
(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))


;; ;; remove pick
;; (define rempick
;;   (lambda (n lat)
;;     (cond
;;      ((zero? (sub1 n)) (cdr lat))
;;      (else
;;       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))



;; remove all the numbers in the lat
(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (no-nums (cdr lat)))
     (else
      (cons (car lat) (no-nums (cdr lat)))))))

;; extract a tup from the lat
(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else
      (all-nums (cdr lat))))))


;; equal atom
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (o= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else
      (eq? a1 a2)))))

;; counts the number of times an atom a appears in a lat
(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else(cond
	   ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
	   (else
	    (occur a (cdr lat))))))))


(define one?
  (lambda (n)
    (o= n 1)))



;; rewrite rempick with one?
(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else
      (cons (car lat) (rempick (sub1 n) (cdr lat)))))))


;; remvoe member a from l 
(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? a (car l)) (rember* a (cdr l)))
       (else
	(cons (car l) (rember* a (cdr l))))))
     (else
      (cons (rember* a (car l)) (rember* a (cdr l)))))))


(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
       (else
	(cons (car l) (insertR* new old (cdr l))))))
     (else
      (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))


(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (add1 (occur* a (cdr l))))
       (else
	(occur* a (cdr l)))))
     (else
      (o+ (occur* a (car l)) (occur* a (cdr l)))))))


(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new (subst* new old (cdr l))))
       (else
	(cons (car l) (subst* new old (cdr l))))))
     (else
      (cons (subst* new old (car l)) (subst* new old (cdr l)))))))


(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
       (else
	(cons (car l) (insertL* new old (cdr l))))))
     (else
      (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))



     
(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) #t)
       (else
	(member* a (cdr l)))))
      (else
       (or (member* a (car l)) (member* a (cdr l)))))))


;; finds the leftmost atom in a non-empty list of S-expressions that dose not contain the empty list.
;; not a *-function
(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else
      (leftmost (car l))))))


;; equal list
;; (define eqlist?
;;   (lambda (l1 l2)
;;     (cond
;;      ;; nine questions
;;      ;; each argument may be either
;;      ;; --empty
;;      ;; an atom consed onto a list
;;      ;; a list consed onto a list
;;      ((and (null? l1) (null? l2)) #t)
;;      ((and (null? l1) (atom? (car l2))) #f)
;;      ((null? l1) #f)

;;      ((and (atom? (car l1)) (null? l2)) #f)
;;      ((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
;;      ((atom? (car l1)) #f)

;;      ((null? l2) #f)
;;      ((atom? (car l2)) #f)
;;      (else
;;       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

;; (define eqlist?
;;   (lambda (l1 l2)
;;     (cond
;;      ((and (null? l1) (null? l2)) #t)
;;      ((or (null? l1) (null? l2)) #f)
;;      ((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
;;      ((or (atom? (car l1)) (atom? (car l2))) #f)
;;      (else
;;       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))


;; equal S-expressions
;; (define equal?
;;   (lambda (s1 s2)
;;     (cond
;;      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
;;      ((atom? s1) #f)
;;      ((atom? s2) #f)
;;      (else
;;       (eqlist? s1 s2)))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else
      (eqlist? s1 s2)))))



;; rewrite eqlists? with equal?
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2))))))) ;need termination


;; (define rember
;;   (lambda (s l)
;;     (cond
;;      ((null? l) (quote ()))
;;      ((atom? (car l))
;;       (cond
;;        ((equal? (car l) s) (cdr l))
;;        (else
;; 	(cons (car l) (rember s (cdr l))))))
;;      (else
;;       (cond
;;        ((equal? (car l) s) (cdr l))
;;        (else
;; 	(cons (car l) (rember s (cdr l)))))))))


;; (defind rember
;;   (lambda (s l)
;;     (cond
;;      ((null? l) (quote ()))
;;      (else
;;       (cond
;;        ((equal? (car l) s) (cdr l))
;;        (else
;; 	(cons (car l) (rember s (cdr l)))))))))



(define rember
  (lambda (s l)
    (cond
     ((null? l) (quote ()))
     ((equal? (car l) s) (cdr l))
     (else
      (cons (car l) (rember s (cdr l)))))))


;; ;; determines whether a representation of an arithmetic expression contains only numbers besides the +, x, and ^.
;; ;; arithemetic expression
;; (define numbered?
;;   (lambda (aexp)
;;     (cond
;;      ((atom? aexp) (number? aexp))
;;      ((eq? (car (cdr aexp)) (quote +)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))
;;      ((eq? (car (cdr aexp)) (quote x)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))
;;      ((eq? (car (cdr aexp)) (quote ^)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))))))
;; ;; Why do we ask four, instead of two, questions about arithmetic expressions?
;; ;; (3 + 5)
;; ;; we condider (1 + 3) as a representation of an arithmetic expression in list form, not as a list itself.


;; representation
;; (1 + 2)
;; since aexp was already understood to be an arithmetic expression
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp)))))))))


;; (1 + 2)
;; ;; natural value
;; (define value
;;   (lambda (aexp)
;;     (cond
;;      ((atom? nexp) nexp)
;;      ((eq? (car (cdr nexp)) (quote +))
;;       (o+ (value (car nexp))
;; 	  (value (car (cdr (cdr nexp)))))))
;;     ((eq? (car (cdr nexp)) (quote x))
;;      (ox (value (car nexp))
;; 	 (value (car (cdr (cdr nexp)))))))
;;   (else
;;    (o^ (value (car nexp))
;;        (value (car (cdr (cdr nexp)))))))


;; ;; (+ 1 2)
;; (define value
;;   (lambda (nexp)
;;     (cond
;;      ((atom? nexp) nexp)
;;      ((eq? (car nexp) (quote +))
;;       (o+ (value (car (cdr nexp)))
;; 	  (value (car (cdr (cdr nexp))))))
;;      ((eq? (car nexp) (quote x))
;;       (ox (value (car (cdr nexp)))
;; 	  (value (car (cdr (cdr nexp))))))
;;      (else
;;       (o^ (value (car (cdr nexp)))
;; 	  (value (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))


(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))


;; (define value
;;   (lambda (nexp)
;;     (cond
;;      ((atom? nexp) nexp)
;;      ((eq? (operator nexp) (quote +))
;;       (o+ (value (1st-sub-exp nexp))
;; 	 (value (2nd-sub-exp nexp))))
;;      ((eq? (operator nexp) (quote x))
;;       (ox (value (1st-sub-exp nexp))
;; 	  (value (2nd-sub-exp nexp))))
;;      (else
;;       (o^ (value (1st-sub-exp nexp))
;; 	  (value (2nd-sub-exp nexp)))))))




;; ;;
;; (define set?
;;   (lambda (lat)
;;     (cond
;;      ((null? lat) #t)
;;      (else
;;       (cond
;;        ((member? (car lat) (cdr lat)) #f)
;;        (else
;; 	(set? (cdr lat))))))))


;;
(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
       ((member? (car lat) (cdr lat)) #f)
       (else
	(set? (cdr lat))))))




;; (define makeset
;;   (lambda (lat)
;;     (cond
;;      ((null? lat) (quote ()))
;;      ((meber? (car lat) (cdr lat))
;;       (makeset (cdr lat)))
;;      (else
;;       (cons (car lat) (makeset (cdr lat)))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cons (car lat)
	    (makeset (multirember (car lat) (cdr lat))))))))


;; (define subset?
;;   (lambda (set1 set2)
;;     (cond
;;      ((null? set1) #t)
;;      (else				;could be simplified
;;       (cond				;could be simpified
;;        ((member? (car set1) set2)
;; 	(subset? (cdr set1) set2))
;;        (else
;; 	#f))))))



;; (define subset?
;;   (lambda (set1 set2)
;;     (cond
;;      ((null? set1) #t)
;;      ((member? (car set1) set2)
;;       (subset? (cdr set1) set2))
;;      (else
;;       #f))))



(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2)
	   (subset? (cdr set1) set2))))))



;; (define eqset?
;;   (lambda (set1 set2)
;;     (cond
;;      ((subset? set1 set2) (subset? set2 set1))
;;      (else #f))))


;; (define eqset?
;;   (lambda (set1 set2)
;;     (cond
;;      (else
;;       (and (subset? set1 set2)
;; 	   (subset? set2 set1))))))


(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
	 (subset? set2 set1))))

;; (define intersect?
;;   (lambda (set1 set2)
;;     (cond
;;      ((null? set1) #f)
;;      ((member? (car set1) set2) #t)
;;      (else
;;       (intersect? (cdr set1) set2)))))



(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else
      (or (member? (car set1) set2)
	  (intersect? (cdr set1) set2))))))


(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) (quote ()))
     ((member? (car set1) set2)
      (cons (car set1) (intersect (cdr set1) set2)))
     (else
      (intersect (cdr set1) set2)))))


(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else
      (cons (car set1)
	    (union (cdr set1) set2))))))

;; returns all the atoms in set1 that are not in set2
(define difference
  (lambda (set1 set2)
    (cond
     ((null? set1) (quote ()))
     ((member? (car set1) set2)
      (difference (cdr set1) set2))
     (else
      (cons (car set1)
	    (difference (cdr set1) set2))))))


;; l-set list of set
;; assuming that the list of sets is non-empty
(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else
      (intersect (car l-set)
		 (intersectall (cdr l-set)))))))


;; pair: a list with only two S-expression
(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else
      #f))))



;; p: pair
(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1
	  (cons s2
		(quote ())))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))


;; rel relation, a set of pairs
;; fun: function
;; all the first elements is a set
(define fun?
  (lambda (rel)
    (set? (firsts rel))))


;; ;; reverse relation
;; (define revrel
;;   (lambda (rel)
;;     (cond
;;     ((null? rel) (quote ()))
;;     (else
;;      (cons (build (second (car rel))
;; 		  (first (car rel)))
;; 	   (reverl (cdr rel)))))))


(define revpair
  (lambda (pair)
    (build (second pair)
	   (first pair))))


(define revrel
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else
      (cons (revpair (car rel))
	    (revrel (cdr rel)))))))



(define seconds
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else
      (cons (car (cdr (car l)))
	    (seconds (cdr l)))))))



(define fullfun?
  (lambda (fun)
    (set? seconds fun)))

;; equal to fullfun?
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))


;; ;; i don't knwo
;; (define bake
;;   (lambda (degrees minutes)
;;     (quote (bake))))

;; (define mix
;;   (lambda ()
;;     (quote (mix))))

;; (define cream
;;   (lambda ()
;;     (quote (cream))))

;; (define cookies
;;   (lambda ()
;;     (bake
;;      (quote (350 degrees))
;;      (quote (12 minutes))
;;      (mix
;;       (quote (walnuts 1 cup))
;;       (quote (chocolate-chips 16 ounces))
;;       (mix
;;        (mix
;; 	(quote (flour 2 cups))
;; 	(quote (oatmeal 2 cups))
;; 	(quote (salt .5 teaspoon))
;; 	(quote (baking-powder 1 teaspoon))
;; 	(quote (baking-soda 1 teaspoon)))
;;        (mix
;; 	(quote (egges 2 large))
;; 	(quote (vanilla 1 teaspoon))
;; 	(cream
;; 	 (quote (butter 1 cup))
;; 	 (quote (sugar 2 cups)))))))))







;; (define rember-f
;;   (lambda (test? a l)
;;     (cond
;;      ((null? l) (quote ()))
;;      (else
;;       (cond
;;        ((test? a (car l)) (cdr l))
;;        (else
;; 	(cons (car l)
;; 	      (rember-f test? a (cdr l)))))))))


;; (define rember-f
;;   (lambda (test?)
;;     (lambda (a l)
;;       (cond
;;        ((null? l) (quote ()))
;;        ((test? (car l) a) (cdr l))
;;        (else
;; 	(cons (car l) ...))))))



(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) (quote ()))
       ((test? (car l) a) (cdr l))
       (else
	(cons (car l)
	      ((rember-f test?) a (cdr l))))))))


;; (define rember-eq?
;;   (rember-f eq?))
;; We do not needto give the name rember-eq? to the function (rember-f eq?)
;; We could have written
;; ((rember-f eq?) a l)


(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((test? (car l) old)
	(cons new (cons old (cdr l))))
       (else
	(cons (car l)
	      ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? (quote ())))
       ((test? (car l) old)
	(cons old (cons new (cdr l))))
       (else
	(cons (car l)
	      ((insertR-f test?) new old (cdr l))))))))


(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))


(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
      ((null? l) (quote ()))
      ((eq? (car l) old)
       (seq new old (cdr l)))
      (else
       (cons (car l)
	     ((insert-g seq) new old (cdr l))))))))

;; (define insertL
;;   (insert-g seqL))

(define insertR
  (insert-g seqR))

;; better, beacuse you do not need to remember as many names
(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst
  (insert-g seqS))



(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x (quote +)) o+)
     ((eq? x (quote x)) ox)
     (else o^))))


(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function (operator nexp))
       (value (1st-sub-exp nexp))
       (value (2nd-sub-exp nexp)))))))


(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) (quote ()))
       ((test? (car lat) a)
	((multirember-f test?) a (cdr lat)))
       (else
	(cons (car lat)
	      ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))


;; where test? is eq?-tuna
(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) (quote ()))
     ((test? (car lat))
      (multiremberT test? (cdr lat)))
     (else
      (cons (car lat)
	    (multiremberT test?
			  (cdr lat)))))))





;; col: colector
(define multiremberEco
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col (quote ()) (quote ())))
     ((eq? (car lat) a)
      (multiremberEco a
		      (cdr lat)
		      (lambda (newlat seen)
			(col newlat
			     (cons (car lat) seen)))))
     (else
      (multiremberEco a
		      (cdr lat)
		      (lambda (newlat seen)
			(col (cons (car lat) newlat)
			     seen)))))))





;; (define a-friend
;;   (lambda (x y)
;;     (null? y)))

;; (define new-firend
;;   (lambda (newlat seen)
;;     (a-friend newlat
;; 	      (cons (car lat) seen))))

;; (define latest-friend
;;   (lambda (newlat seen)
;;     (a-friend (cons (car lat) newlat)
;; 	      seen)))

;; (define last-friend
;;   (lambda (x y)
;;     (length x)))



;; multiinsertLR inserts new to the left of oldL and to the right of oldR in lat
;; if oldL and oldR are different.
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) oldL)
      (cons new
	    (cons oldL
		  (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR
	    (cons new
		  (multiinsertLR new oldL oldR (cdr lat)))))
     (else
      (cons (car lat)
	    (multiinsertLR new oldL oldR (cdr lat)))))))


;; (define multiinsertLREco
;;   (lambda (new oldL oldR lat col)
;;     (cond
;;      ((null? lat)
;;       (col (quote ()) 0 0))
;;      ((eq? (car lat) oldL)
;;       (multiinsertLREco new oldL oldR (cdr lat)
;; 			(lambda (newlat L R)
;; 			  ...)))
;;      ((eq? (car lat) oldR)
;;       (multiinsertLREco new oldL oldR (cdr lat)
;; 			(lambda (newlat L R)
;; 			  ...)))
;;      (else
;;       (multiinsertLREco new oldL oldR (cdr lat)
;; 			(lambda (newlat L R)
;; 			  ...))))))



(define multiinsertLREco
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col (quote ()) 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLREco new oldL oldR (cdr lat)
			(lambda (newlat L R)
			  (col (cons new
				     (cons oldL newlat))
			       (add1 L)
			       R))))
     ((eq? (car lat) oldR)
      (multiinsertLREco new oldL oldR (cdr lat)
			(lambda (newlat L R)
			  (col (cons oldR
				     (cons new newlat))
			       L
			       (add1 R)))))
     (else
      (multiinsertLREco new oldL oldR (cdr lat)
			(lambda (newlat L R)
			  (col (cons (car lat) newlat) L R)))))))



(define even?
  (lambda (n)
    (= (ox (o/ n 2) 2) n)))


(define evens-only*
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((even? (car l))
	(cons (car l)
	      (evens-only* (cdr l))))
       (else
	(evens-only* (cdr l)))))
     (else
      (cons (evens-only* (car l))
	    (evens-only* (cdr l)))))))



;; builds a nested list of even numbers
;; by removing the odd ones from its argument
;; and simultaneously multiplies the even numbers
;; and sums up the odd numbers;
					; that occur in its argument
(define evens-only*Eco
  (lambda (l col)
    (cond
     ((null? l)
      (col (quote ()) 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
	(evens-only*Eco (cdr l)
		     (lambda (newl p s)
		       (col (cons (car l) newl)
			    (ox (car l) p)
			    s))))
       (else
	(evens-only*Eco (cdr l)
		     (lambda (newl p s)
		       (col newl p (o+ (car l) s)))))))
     (else
      (evens-only*Eco (car l)
		   (lambda (al ap as)
		     (evens-only*Eco (cdr l)
				     (lambda (dl dp ds)
				       (col (cons al dl)
					    (ox ap dp)
					    (o+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
	  (cons product
		newl))))





;; this is called partial function
;; the above functions are called total function
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))


;; sorn: symbol or number
(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     (else
      (eq? sorn a)))))




(define eternity
  (lambda (x)
    (eternity x)))



;; takes a pair whose first component is a pair and builds a pair
;; by shifting the second part of the first component into the second component
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))


(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else
      (build (first pora)
             (align (second pora)))))))


;; counts the number of atoms in align's arguments
(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (o+ (length* (first pora))
          (length* (second pora)))))))



(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (o+ (ox (weight* (first pora)) 2)
          (weight* (second pora)))))))



(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (shuffle (revpair pora)))
     (else
      (build (first pora)
             (shuffle (second pora)))))))

;; Lothar Collatz (1910-1990)
(define C
  (lambda (n)
    (cond
     ((one? n) 1)
     (else
      (cond
       ((even? n) (C (o/ n 2)))
       (else
        (C (add1 (ox 3 n)))))))))


;; Wilhelm Ackermann (1853-1946)
(define A
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (A (sub1 n) 1))
     (else
      (A (sub1 n)
         (A n (sub1 m)))))))
;; What dose A have in common with shuffle and looking?
;; A's arguments, like shuffle's and looking's do not necessarily decrease for the recursion.

;; Dose A always give an answer?
;; Yes, it is total.

;; What is (A 4 3)?
;; For all practical purposes, there is no answer.


;; (define will-stop?
;;   (lambda (f)
;;     ...))




(define last-try
  (lambda (x)
    (and (will-stop? last-try)
         (eternity x))))

;; If we can define will-stop?, then (will-stop? last-try) must yield
;; either #t or #f. But it cannot -- due to the very definition of
;; what will-stop? is supposed to do.
;; This must mean that will-stop? cannot be defined.

;; This is unique.
;; It makes will-stop? the first function that we can describe precisely
;; but cannot define in our language.
;; Alan M. Turing (1912-1954)
;; Kurt Godel (1906-1978)



;; This is the function length
;; (define length
;;   (lambda (l)
;;     (cond
;;      ((null? l) 0)
;;      (else
;;       (add1 (length (cdr l)))))))

;; What if we didn't have (define ...) anymore?
;; Without (define ...) nothing, and especially not the body of length, could refer to length.



;; What does this function do?
(lambda (l)
  (cond
   ((null? l) 0)
   (else
    (add1 (eternity (cdr l))))))
;; It determines the length of the empty list and nothing else.

;; Suppose we could name this new function. What would be a good name?
;; length0, because the function can only be determine the length of the empty list.


;; write a function that determines the length of lists that contain one or fewer items
(lambda (l)
  (cond
   ((null? 0)
    (else
     (add1 (length0 (cdr l)))))))

;; but (define ...) doesn't work for length0
;; so, replace length0 by its definition
(lambda (l)
  (cond
   ((null? l) 0)
   (else
    (add1
     ((lambda (l)
        (cond
         ((null? l) 0)
         (else
          (add1 (eternity (cdr l))))))
      (cdr l))))))

;; a good name for this function?
;; length<=1                               

;; length<=2
(lambda (l)
  (cond
   ((null? l) 0)
   (else
    (add1
     ((lambda (l)
        (cond
         ((null? l) 0)
         (else
          (add1
           ((lambda (l)
              (cond
               ((null? l) 0)
               (else
                (add1
                 (eternity
                  (cdr l))))))
            (cdr l))))))
      (cdr l))))))

;; we have seen how to determine the length of a list no items, with no more than one item, with no more than two items, and so on. How could we get the function length back?
;; If we could write an infinite function in the style of length0, length<=1, length<=2, ..., then we could write length00, which would determine the length of all lists that we can make.

;; but we can't write an infinite function

;; we still have all these repetitions and patterns in these functions

;; what do these patterns look like?
;; all these programs contain a function that looks like length. Perhaps we should be abstract out this function: see the Ninth Commandment.

;; Let's do it!
;; We need a function that looks just like length but starts with
;; (lambda (length) ...)






;; ;; Let's do it!
;; ;; We need a function that looks just like length but starts with (lambda (length) ...)

;; ((lambda (length)
;;    (lambda (l)
;;      (cond
;;       ((null? l) 0)
;;       (else add1 (length (cdr l))))))
;;  eternity)
;; ;; It creates length0


;; ;; length<=1
;; ((lambda (f)
;;    (lambda (l)
;;      (cond
;;       ((null? l) 0)
;;       (else
;;        (add1 (f (cdr l))))))
;;    ((lambda (g)
;;       (lambda (l)
;; 	(cond
;; 	 ((null? l) 0)
;; 	 (else
;; 	  (add1 (g (cdr l)))))))
;;     eternity))

;;  ;; Do we have to use length to name the argument?
;;  ;; No, we just used f and g. As long as we are consistent, everything's okay.

;;  ;; length<=3
;;  ((lambda (length)
;;     (lambda (l)
;;       (cond 
;;        ((null? l) 0)
;;        (else
;; 	(add1 (length (cdr l)))))))
;;   ((lambda (length)
;;      (lambda (l)
;;        (cond
;; 	((null? l) 0)
;; 	(else
;; 	 (add1 (length (cdr l)))))))
;;    ((lambda (length)
;;       (lambda (l)
;; 	(cond
;; 	 ((null? l) 0)
;; 	 (else
;; 	  (add1 (length (cdr l)))))))
;;     eternity)))

;;  ;; there are still repetitions
;;  ;; let's get rid of them

;;  ;; Where should we start?
;;  ;; Name the function that takes length as an argument and that returns a functions that looks like length

;;  ;; a good name for this function
;;  ;; mk-length for "make length"

;;  ;; length0
;;  ((lambda (mk-length)
;;     (mk-length eternity))
;;   (lambda (length)
;;     (lambda (l)
;;       (cond
;;        ((null? l) 0)
;;        (else
;; 	(add1 (length (cdr l))))))))

;;  ;; length<=1
;;  ((lambda (mk-length)
;;     (mk-length
;;      (mk-length eternity)))
;;   (lambda (length)
;;     (lambda (l)
;;       (cond
;;        ((null? l) 0)
;;        (else
;; 	(add1 (length (cdr l))))))))

;;  ;; length<=2
;;  ((lambda (mk-length)
;;     (mk-length
;;      (mk-length
;;       (mk-length eternity))))
;;   (lambda (length)
;;     (lambda (l)
;;       (cond
;;        ((null? l) 0)
;;        (else
;; 	(add1 (length (cdr l))))))))

;;  ;; length<=3
;;  ((lambda (mk-length)
;;     (mk-length
;;      (mk-length
;;       (mk-length
;;        (mk-length eternity)))))
;;   (lambda (length)
;;     (lambda (l)
;;       (cond
;;        ((null? l) 0)
;;        (else
;; 	(add1 (length (cdr l))))))))

;;  ;; what is recursion like?
;;  ;; It is like an infinite tower of applications of mk-length to an arbitary function.

;;  ;; since nobody cares about what function we pass to mk-length we could pass it mk-length initially
;;  ;; That's the right idea. And then we invoke mk-length on eternity and the result of this on cdr so that we get one more piece of the tower.

;;  ((lambda (mk-length)
;;     (mk-length mk-length))
;;   (lambda (length)
;;     (lambda (l)
;;       (cond
;;        ((null? l) 0)
;;        (else
;; 	(add1 (length (cdr l))))))))

;;  ;; we could even use mk-length instead of length.

;;  ((lambda (mk-length)
;;     (mk-length mk-length))
;;   (lambda (mk-length)
;;     (lambda (l)
;;       (cond
;;        ((null? l) 0)
;;        (else
;; 	(add1 (mk-length (cdr l))))))))

;;  ;; Why would we want to do that?
;;  ;; All names are equal, but some names are more equal than others.


;;  ;; Now that mk-length is passed to mk-length can we use the argument to create an additional recursive use?
;;  ;; Yes, when we apply mk-length once, we get length<=1
;;  ((lambda (mk-length)
;;     (mk-length mk-length))
;;   (lambda (mk-length)
;;     (lambda (l)
;;       (cond
;;        ((null? l) 0)
;;        (else
;; 	(add1 
;; 	 ((mk-length eternity)
;; 	  (cdr l))))))))


;;  ;; What would you call this function?
;;  ((lambda (mk-length)
;;     (mk-length mk-length))
;;   (lambda (mk-length)
;;     (lambda (l)
;;       (cond
;;        ((null? l) 0)
;;        (else
;; 	(add1 
;; 	 ((mk-length mk-length)
;; 	  (cdr l))))))))
;;  ;; It is length

;;  ;; How does it work?
;;  ;; It keeps adding recursive uses by passing mk-length to itself, just as it is about to expire.

;;  ;; One problem is left: it no longer contains the function that looks like length
;;  ;; We could extract this new application of mk-length to itself and call it length
;;  ((lambda (mk-length)
;;     (mk-length mk-length))
;;   (lambda (mk-length)
;;     (lambda (l)
;;       (cond
;;        ((null? l) 0)
;;        (else
;; 	(add1 
;; 	 ((mk-length mk-length)		;this part
;; 	  (cdr l))))))))
 
;;  ;; Why?
;;  ;; Because it really makes the function length

;;  ((lambda (mk-length)
;;     (mk-length mk-length))
;;   (lambda (mk-length)
;;     (lambda (l)
;;       (cond
;;        ((null? l) 0)
;;        (else
;; 	(add1
;; 	 (length (cdr l)))))))
;;   (mk-length mk-length))))



;; ;; If f is a function of one argument, is (lambda (x) (f x)) a function of one augument?
;; ;; Yes it is.

;; ;; If (mk-length mk-length) returns a function of one argument, does 
;; (lambda (x)
;;   ((mk-length mk-length) x))
;; ;; return a function of one argument?
;; ;; Actually,
;; (lambda (x)
;;   ((mk-length mk-length) x)
;; ;; is a function!


;;   ;; do this to the application of mk-length of itself
;;   ((lambda (mk-length)
;;      (mk-length mk-length))
;;    (lambda (mk-length)
;;      (lambda (l)
;;        (cond
;; 	((null? l) 0)
;; 	(else
;; 	 (add1
;; 	  (lambda (x)			;
;; 	    ((mk-length mk-length) x)	;
;; 	    (cdr l))))))))

;;   ;; move out the new function so that we get length back
;;   ((lambda (mk-length)
;;      (mk-length mk-length))
;;    (lambda (mk-length)
;;      (
;;       (lambda (length)			;
;; 	(lambda (l)			;
;; 	  (cond				;
;; 	   ((null? l) 0)		;
;; 	   (else			;
;; 	    (add1 (length (cdr l)))))))	;
;;       (lambda (x)
;; 	((mk-length mk-length) x)))))


;; ;; can we extract the function with comment on right that looks like length and give it a name?
;; ;; yes, it does not depend on the mk-lenght at all?

;; ((lambda (le)
;;    ((lambda (mk-length)
;;       (mk-length mk-length)
;;       (lambda (mk-length)
;; 	(le (lambda (x)
;; 	      ((mk-length mk-length) x)))))))
;;  (lambda (length) 
;;    (lambda (l)
;;      (cond
;;       ((null? l) 0)
;;       (else
;;        (add1 (length (cdr l))))))))

;; ;; What did we actually get back?
;; ;; we extracted the original function mk-length.

;; ;; separate the function that makes length from the function that looks like length
;; (lambda (le)
;;   ((lambda (mk-length)
;;      (mk-length mk-length))
;;    (lambda (mk-length)
;;      (le (lambda (x)
;; 	   ((mk-length mk-length) x))))))

;; ;; Does this function have a name?
;; ;; Yes, it is called the application-order Y combinator.
;; (define Y
;;   (lambda (le)
;;     ((lambda (f) (f f))
;;      (lambda (f)
;;        (le (lambda (x) ((f f) x)))))))

;; ;; Does (define ...) work again?
;; ;; Sure, now that we know what recursion is.






;; build an entry from a set of names and a list of values
(define new-entry
  build)




(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
			  (first entry)
			  (second entry)
			  (entry-f))))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))	;entry-f deal with name not find situation
     ((eq? (car names) name)
      (car values))
     (else
      (lookup-in-entry-help (cdr names)
			    (cdr values)
			    entry-f)))))

;; takes an entry and a table and creates a new table by putting the new entry
;; in front of the old table.
(define extend-table
  cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else
      (lookup-in-entry name
		       (car table) 
		       (lambda (name)	;the action to take when the name
					;is not found in the first entry
			 (lookup-in-table name
					  (cdr table)
					  table-f)))))))
		       



;; a function that produces the correct action (or function) for each possible S-expression:
(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e (quote cons)) *const)
     ((eq? e (quote car)) *const)
     ((eq? e (quote cdr)) *const)
     ((eq? e (quote null?)) *const)
     ((eq? e (quote eq?)) *const)
     ((eq? e (quote atom?)) *const)
     ((eq? e (quote zero?)) *const)
     ((eq? e (quote add1)) *const)
     ((eq? e (quote sub1)) *const)
     ((eq? e (quote number?)) *const)
     (else
      *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) (quote quote))
	*quote)
       ((eq? (car e) (quote lambda))
	*lambda)
       ((eq? (car e) (quote cond))
	*cond)
       (else *application)))
     (else
      *application))))


;; assuming that expression-to-action works,
;; we can use it to define value and meaning
(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;; (quote ()) is the definition of value is the empty table
;; the function value together with all the functions is uses, is called an interpreter.


;; How man arguments should actions take according to the above?
;; Two, the expression e and a table.

;; the action for constants
(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else
      (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (test-of e)))

(define test-of
  second)

;; Why do we need the table?
;; To remeber the values of identifiers.

;; Given that the table contains the value of identifiers
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))


;; Here is initial-table
(define initial-table
  (lambda (name)
    (car (quote ()))))
;; When is it used?
;; Let's hope never.

;; non-primitives are defined by their arguments and their function bodies.
;; So when we want to use a non-primitive we need to remeber its formal arguemnts
;; and its function body.
;; Fortunately this is just the cdr of a lambda expression.

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
	   (cons table (cdr e)))))

;; it is probably a good idea of define some help functions for getting back the parts in this three element list (i.e., the table, the formal arguments and the body)
(define table-of
  first)

(define formals-of
  second)

(define body-of
  third)

 

;; (cond ...) is a special form that takes any number of cond-lines. It considers each line in turn. If the question part on the left is false, it looks at the rest of the lines. Otherwise it proceeds to answer the right part. If it sees an else-line, it treats that cond-line as if its question part were true.
(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table)
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table)
       (else
	(evcon (cdr lines) table)))))))

(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x (quote else)))
     (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)


;; before we can apply a function, we have to get the meaning of all of its arguments.
;; a function evlis that takes a list of (representations of) arguments and a table, and returns a list of composed of the meaning of each argument.
(define evlis
  (lambda (args table)
    (cond
     ((null? args) (quote ()))
     (else
      (cons (meaning (car args) table)
	    (evlis (cdr args) table))))))

;; apply the meaning of the function to the meaning of the arguments.
(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

;; How many different kinds of functions are there?
;; Two: primitive and non-primitives

;; What are the two representations of functions?
;; (primitive primitive-name) and
;; (non-primitive (table formals body))
;; The list (table formals body) is called a closure record.

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

;; primitive fun-name
;; non-primitive fun-name
(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive (second fun) vals))
     ((non-primitive? fun)
      (apply-closure (second fun) vals)))))


(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name (quote cons))
      (cons (first vals) (second vals)))
     ((eq? name (quote car))
      (car (first vals)))
     ((eq? name (quote cdr))
      (cdr (first vals)))
     ((eq? name (quote null?))
      (null? (first vals)))
     ((eq? name (quote eq?))
      (eq? (first vals) (second vals)))
     ((eq? name (quote atom?))
      (:atom? (first vals)))
     ((eq? name (quote zero?))
      (zero? (first vals)))
     ((eq? name (quote add1))
      (add1 (first vals)))
     ((eq? name (quote sub1))
      (sub1 (first vals)))
     ((eq? name (quote number?))
      (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) (quote primitive)) #t)
     ((eq? (car x) (quote non-primitve)) #t)
     (else
      #f))))

;; How could we find the result of (f a b) where
;; f is (lambda (x y) (cons x y))
;; a is 1
;; and
;; b is (2)

;; We know what to do to find the meaning of 
;; (cons x y)
;; where 
;; table is (((x y) (1 (2))))

;; Why can we do this?
;; Here, we don't need apply-clusre.

;; generalize the last two steps
;; Applying a non-primitive function --a closure-- to a list of values is the same as finding the meaning of the closure's body with its table extended by an entry of the form
;; (formals values)
;; In this entry, formals is the formals of the closure and values is the result of elis.

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
	     (extend-table
	      (new-entry
	       (formals-of closure)
	       vals)
	      (table-of closure)))))

