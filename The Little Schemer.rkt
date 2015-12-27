#lang racket

#|
The Little Schemer - 4th Ed.
Daniel P. Friedman and Matthias Felleisen
|#

#! 1. Toys
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
#! End 1. Toys

#! 2. Do It, Do It Again, and Again, and Again... ****************************************
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat) 
    (cond 
      ((null? lat) #f) 
      ((equal? a (car lat)) #t) 
      (else (member? a (cdr lat))))))
#! End 2. Do It, Do It Again, and Again, and Again... ************************************

#! 3. Cons the Magnificent ***************************************************************
#! A few of the functions here, rember, subst, insertR and insertL have been redefined
#!   in Chapter 8. Lambda the Ultimate.  So, I had to omit them here.
(define firsts
  (lambda (l)
    (cond
      ((null? l) l)
      (else (cons (caar l) (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) l)
      (else (cons (cadar l) (seconds (cdr l)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) lat)
      ((or (equal? o1 (car lat)) (equal? o2 (car lat))) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) lat)
      ((equal? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      ((equal? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      ((equal? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      ((equal? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))
#! End 3. Cons the Magnificent ***********************************************************

#! 4. Numbers Games **********************************************************************
(define add1 (lambda (n) (+ n 1)))
(define sub1 (lambda (n) (- n 1)))

(define o+
  (lambda (n m)
    (cond
      ((zero? n) m)
      (else (o+ (sub1 n) (add1 m))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (o- (sub1 n) (sub1 m))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define ox
  (lambda (n m)
    (cond 
      ((zero? m) 0)
      (else (o+ n (ox n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) '())
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
      ((and (zero? m) (zero? n)) #t)
      ((zero? m) #f)
      ((zero? n) #f)
      (else (o= (sub1 n) (sub1 m))))))

(define o^
  (lambda (n m)
    (cond 
      ((zero? m) 1)
      (else (* n (o^ n (sub1 m)))))))

(define mylen
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (+ 1 (mylen (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (- n 1)) (car lat))
      (else (pick (- n 1) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (- n 1) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((equal? a (car lat)) (+ 1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (= n 1)))
#! End 4. Numbers Games ******************************************************************

#! 5. *Oh My Gawd*: It's Full of Stars ***************************************************
(define rember*
  (lambda (a l)
    (cond
      ((null? l) l)
      ((pair? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
      ((equal? a (car l)) (rember* a (cdr l)))
      (else (cons (car l) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) l)
      ((pair? (car l)) (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
      ((equal? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
      (else (cons (car l) (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((pair? (car l)) (+ (occur* a (car l)) (occur* a (cdr l))))
      ((equal? a (car l)) (+ 1 (occur* a (cdr l))))
      (else (occur* a (cdr l))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) l)
      ((pair? (car l)) (cons (subst* new old (car l)) (subst* new old (cdr l))))
      ((equal? old (car l)) (cons new (subst* new old (cdr l))))
      (else (cons (car l) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) l)
      ((pair? (car l)) (cons (insertL* new old (car l)) (insertL* new old (cdr l))))
      ((equal? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
      (else (cons (car l) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((pair? (car l)) (or (member* a (car l)) (member* a (cdr l))))
      ((equal? a (car l)) #t)
      (else (member* a (cdr l))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) car l)
      (else (leftmost (car l))))))
#! End 5. *Oh My Gawd*: It's Full of Stars ***********************************************

#! 6. Shadows ****************************************************************************
(define operator? (lambda (op) (or (equal? op '+) (equal? op 'x) (equal? op '^))))

(define numbered?
  (lambda (aexp)
    (cond
      ((null? aexp) #t)
      ((operator? aexp) #t)
      ((number? aexp) #t)
      ((pair? aexp) (and (numbered? (car aexp)) (numbered? (cdr aexp))))
      (else #f))))

(define aexpvalue
  (lambda (aexp)
    (cond
      ((not (numbered? aexp)) #f)
      ((operator? aexp) (cond ((eqv? aexp '+) +) ((eqv? aexp 'x) *) ((eqv? aexp '^) expt)))
      ((number? aexp) aexp)
      (else ((aexpvalue (car aexp)) (aexpvalue (cadr aexp)) (aexpvalue (caddr aexp)))))))
#! End 6. Shadows ************************************************************************

#! 7. Friends and Relations **************************************************************
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) lat)
      (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

(define eqset? (lambda (set1 set2) (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set2) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) set1)
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(define fun? (lambda (rel) (set? (firsts rel))))

(define a-pair?
  (lambda (x)
    (cond ((atom? x) #f) ((null? x) #f) ((null? (cdr x)) #f) ((null? (cdr (cdr x))) #t) (else #f))))

(define revpair (lambda (pair) (list (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((empty? rel) rel)
      (else (cons (list (second (car rel)) (first (car rel))) (revrel (cdr rel)))))))

(define fullfun? (lambda (rel) (and (set? (firsts rel)) (set? (seconds rel)))))
#! End 7. Friends and Relations **********************************************************

#! 8. Lambda the Ultimate ****************************************************************
(define eq?-c (lambda (a) (lambda (x) (eq? a x))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) l)
        ((test? a (car l)) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define insert-g
  (lambda (test? seq)
    (lambda (new old lat)
      (cond
        ((null? lat) lat)
        ((test? old (car lat)) (seq new old (cdr lat)))
        (else (cons (car lat) ((insert-g test? seq) new old (cdr lat))))))))

(define insertR (insert-g equal? (lambda (new old lat) (list* old new lat))))
(define insertL (insert-g equal? (lambda (new old lat) (list* new old lat))))
(define subst (insert-g equal? (lambda (new old lat) (list* new lat))))
(define rember (lambda (a lat) ((insert-g equal? (lambda (new old lat) lat)) #f a lat)))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) lat)
        ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq? (multirember-f eq?))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) lat)
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((equal? (car lat) a) (multirember&co a (cdr lat) (lambda (newlat seen) (col newlat (cons (car lat) seen)))))
      (else (multirember&co a (cdr lat) (lambda (newlat seen) (col (cons (car lat) newlat) seen)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) lat)
      ((equal? (car lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((equal? (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((equal? oldL (car lat)) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (l ln rn) (col (cons new (cons oldL l)) (+ 1 ln) rn))))
      ((equal? oldR (car lat)) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (l ln rn) (col (cons oldR (cons new l)) ln (+ 1 rn)))))
      (else (multiinsertLR&co new oldL oldR (cdr lat) (lambda (l ln rn) (col (cons (car lat) l) ln rn)))))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) l)
      ((pair? (car l)) (cons (evens-only* (car l)) (evens-only* (cdr l))))
      ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
      (else (evens-only* (cdr l))))))

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l)) (cond ((even? (car l)) (evens-only*&co (cdr l) (lambda (newl p s) (col (cons (car l) newl) (* p (car l)) s))))
                             (else (evens-only*&co (cdr l) (lambda (newl p s) (col newl p (+ s (car l))))))))
      (else (evens-only*&co (cdr l) (lambda (newl p s) (evens-only*&co (car l) (lambda (subl subp subs) (col (cons subl newl) (* subp p) (+ subs s))))))))))
#! End 8. Lambda the Ultimate ************************************************************

#! 9. ... and Again, and Again, and Again, ... *******************************************
(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define shift
  (lambda (pair)
    (list (first (first pair)) (list (second (first pair)) (second pair)))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora)))
      (else (list (first pora) (align (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (shuffle (revpair pora)))
      (else (list (first pora) (shuffle (second pora)))))))

(define collatz
  (lambda (n col)
    (cond
      ((= n 1) (col 0))
      (else
       (cond
         ((even? n) (collatz (/ n 2) (lambda (oneness) (col (+ 1 oneness)))))
         (else (collatz (+ 1 (* 3 n)) (lambda (oneness) (col (+ 1 oneness))))))))))

(define ackermann
  (lambda (n m)
    (cond
      ((zero? n) (+ 1 m))
      ((zero? m) (ackermann (- n 1) 1))
      (else (ackermann (- n 1) (ackermann n (- m 1)))))))

(define eternity (lambda (x) (eternity x)))

(define l0
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ 1 (eternity (cdr l)))))))

(define l1
  (lambda (l)
    (cond
      ((null? l ) 0)
      (else (+ 1 ((lambda (l)
                   (cond
                     ((null? l) 0)
                     (else (+ 1 (eternity (cdr l))))))(cdr l)))))))

(define l2
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ 1 ((lambda (l)
                    (cond
                      ((null? l) 0)
                      (else (+ 1 ((lambda (l)
                                    (cond
                                      ((null? l) 0)
                                      (else (+ 1 (eternity (cdr l))))))(cdr l))))))(cdr l)))))))

(define ll0
  ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (+ 1 (length (cdr l)))))))
   eternity))

(define ll1
  ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (+ 1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (+ 1 (length (cdr l)))))))
    eternity)))

(define ll2
  ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (+ 1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (+ 1 (length (cdr l)))))))
    ((lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (+ 1 (length (cdr l)))))))
     eternity))))

(define lll0
  ((lambda (mk-length) (mk-length eternity))
   (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (+ 1 (length (cdr l)))))))))

(define lll1
  ((lambda (mk-length) (mk-length (mk-length eternity)))
   (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (+ 1 (length (cdr l)))))))))

(define lll2
  ((lambda (mk-length) (mk-length (mk-length (mk-length eternity))))
   (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (+ 1 (length (cdr l)))))))))

(define lll3
  ((lambda (mk-length) (mk-length (mk-length (mk-length (mk-length eternity)))))
   (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (+ 1 (length (cdr l)))))))))

#! the applicative-order Y combinator for the function length
(define mylength
  ((lambda (le)
     ((lambda (mk-length) (mk-length mk-length))
      (lambda (mk-length) (le (lambda (x) ((mk-length mk-length) x))))))
     (lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (+ 1 (length (cdr l)))))))))
#! End 9. ... and Again, and Again, and Again, ... ***************************************

#! 10. What Is the Value of All of This? *************************************************
(define lookup-in-entry
  (lambda (name entry entry-f)
    (cond
      ((null? (car entry)) (entry-f name))
      ((equal? name (caar entry)) (caadr entry))
      (else (lookup-in-entry name (list (cdar entry) (cdadr entry)) entry-f)))))

(define new-entry list)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table) (lambda (name) (lookup-in-table name (cdr table) table-f)))))))

(define extend-table cons)

(define value (lambda (e) (meaning e '())))

(define meaning (lambda (e table) ((expression-to-action e) e table)))

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
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e)) (cond
                         ((eq? (car e) 'quote) *quote)
                         ((eq? (car e) 'lambda) *lambda)
                         ((eq? (car e) 'cond) *cond)
                         (else *application)))
      (else *application))))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (list 'primitive e)))))

(define text-of second)

(define *quote
  (lambda (e table)
    (text-of e)))

(define initial-table
  (lambda (name)
    (car '())))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define *lambda
  (lambda (e table)
    (list 'non-primitive (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x 'else))
      (else #f))))

(define question-of first)
(define answer-of second)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define cond-lines-of cdr)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define evlis
  (lambda (args table)
    (cond
      ((null? args) '())
      (else (cons (meaning (car args) table) (evlis (cdr args) table))))))

(define function-of car)
(define arguments-of cdr)
(define primitive? (lambda (l) (eq? (first l) 'primitive)))
(define non-primitive? (lambda (l) (eq? (first l) 'non-primitive)))

(define *application
  (lambda (e table)
    (my-apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define my-apply
  (lambda (fun vals)
    (cond
      ((primitive? fun) (apply-primitive (second fun) vals))
      ((non-primitive? fun) (apply-closure (second fun) vals)))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive) #t)
      (else #f))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons) (cons (first vals) (second vals)))
      ((eq? name 'car) (car (first vals)))
      ((eq? name 'cdr) (cdr (first vals)))
      ((eq? name 'null) (null? (first vals)))
      ((eq? name 'eq?) (eq? (first vals) (second vals)))
      ((eq? name 'atom?) (:atom? (first vals)))
      ((eq? name 'zero?) (zero? (first vals)))
      ((eq? name 'add1) (add1 (first vals)))
      ((eq? name 'sub1) (sub1 (first vals)))
      ((eq? name 'number?) (number? (first vals))))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure) (extend-table (new-entry (formals-of closure) vals) (table-of closure)))))
#! End 10. What Is the Value of All of This? *********************************************

#| End
The Little Schemer - 4th Ed.
Daniel P. Friedman and Matthias Felleisen
|#
