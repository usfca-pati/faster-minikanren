#lang racket

(require "mk.rkt")

(provide (all-defined-out))

;; peano numerals, b/c miniKanren doesn't ship with arithmetic

(define (zeroo x) (== '() x))
(define (poso x) (fresh (a b) (== `(,a . ,b) x)))
(define (to-peano n)
  (cond
    ((= n 0) '())
    (else `(,n . ,(to-peano (- n 1))))))

(define (+1o a b) (fresh (x) (== b `(,x . ,a))))

(define (gt a b)
  (condu
   ((zeroo b) (poso a))
   ((fresh (a2 b2)
           (+1o a2 a)
           (+1o b2 b)
           (gt a2 b2)))))

(define (non-empty l) (poso l))

(define (halfo n n/2)
  (fresh (a b c c/2)
         (condu
          ((zeroo n) (zeroo n/2))
          ((== n `(,a)) (zeroo n/2))
          ((== n `(,a ,b . ,c))
           (halfo c c/2)
           (== n/2 `(,a . ,c/2))))))

(define (mylength list len)
  (condu
   ((== list '()) (zeroo len))
   ((fresh (h t l)
           (== list `(,h . ,t))
           (mylength t l)
           (+1o l len)
           ))))

(define (myappend l1 l2 l3)
  (fresh (h t y)
         (condu
          ((== l1 '()) (== l2 l3))
          ((== l1 `(,h . ,t))
           (== `(,h . ,y) l3)
           (myappend t l2 y)))))

(define (dupo n l lout)
  (cond
    ((= n 0) (== l lout))
    (else
     (fresh (l2)
            (myappend l l l2)
            (dupo (- n 1) l2 lout)))))

(define (split l1 n left-half right-half)
  (fresh (n1 h t o)
         (condu
          ((zeroo n) (== l1 right-half) (== '() left-half))
          ((+1o n1 n) (== l1 `(,h . ,t))
                      (== left-half `(,h . ,o))
                      (split t n1 o right-half)))))

(define (merge-sort l sorted)
  (fresh (x a b c n n/2 left-half right-half sorted-l sorted-r)
         (condu
          ((== l '()) (== sorted '()))
          ((== l `(,x)) (== sorted `(,x)))
          ((== l `(,a ,b . ,c))
           (mylength l n)
           (halfo n n/2)
           (split l n/2 left-half right-half)
           (merge-sort left-half sorted-l)
           (merge-sort right-half sorted-r)
           (merge sorted-l sorted-r sorted)))))

(define (merge l1 l2 merged)
  (fresh (h1 t1 h2 t2 t)
         (condu
          ((== l1 '()) (== l2 merged))
          ((== l2 '()) (== l1 merged) (non-empty l1))
          ((== l1 `(,h1 . ,t1))
           (== l2 `(,h2 . ,t2))
           (condu
            ((== h1 h2) (== merged `(,h1 . ,t))
                        (merge t1 `(,h2 . ,t2) t))
            ((gt h2 h1) (== merged `(,h1 . ,t))
                        (merge t1 `(,h2 . ,t2) t))
            ((gt h1 h2) (== merged `(,h2 . ,t))
                        (merge `(,h1 . ,t1) t2 t)))))))

(define my-list (map to-peano '(3 4 7 6 5 2 8 1 0 9)))

(define (build n l)
  (if (= n 0)
      (== l '())
      (fresh (a b c)
             (== l `(,a . ,b))
             (conde
              ((== a 3))
              ((== a b) (== a '())))
             (== c l)
             (build (- n 1) b))))

              
(define (benchmark n)
  (time (run 1 (q) (fresh (l sorted) (dupo n my-list l) (merge-sort l sorted)))))

(define success (== 'a 'a))

(define (run-n-times m n)
  (define (go n q)
    (if (zero? n)
        success
        (fresh (l sorted) (dupo m my-list l) (merge-sort l sorted)
               (== sorted q)
               (go (- n 1) q))))
  (time (run 1 (q) (go n q))))

(define (run-independently m n)
  (define (go n)
    (unless (< n 1)
      (run 1 (q) (fresh (l sorted) (dupo m my-list l) (merge-sort l sorted)))
      (go (- n 1))))
  (time (go n)))

(require "hamt/weak-hamt.rkt")
(start-will-executor!)
(collect-garbage)
(run-n-times 2 100)
