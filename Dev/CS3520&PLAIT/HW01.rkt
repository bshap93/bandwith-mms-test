#lang plait
;; HW01 -- Name: Ben Shaprio, uID: u1195235

(print-only-errors #t)

(define-type Tree
  (leaf [val : Number])
  (node [val : Number]
        [left : Tree]
        [right : Tree]))

;; Part 1 — Sum

(define (sum [tree : Tree]) : Number
  (type-case Tree tree
    [(leaf n) n]
    [(node n l r) (+ n (+ (sum l) (sum r)))]))

(test (sum (leaf 4)) 4)
(test (sum (node 7 (leaf 0) (leaf 0))) 7)
(test (sum (node 5 (leaf 6) (leaf 7))) 18)
(test (sum (node 7 (node 5 (leaf 6) (leaf 7)) (leaf 2))) 27)


;; Part 2 — Negate

(define (negate [tree : Tree]) : Tree
  (type-case Tree tree
    [(leaf n) (leaf (- 0 n))]
    [(node n l r) (node (- 0 n) (negate l) (negate r))]))

(test (negate (leaf 4)) (leaf -4))
(test (negate (node 7 (leaf 0) (leaf 0))) (node -7 (leaf -0) (leaf -0)))
(test (negate (node 5 (leaf 6) (leaf 7))) (node -5 (leaf -6) (leaf -7)))
(test (negate (node -5 (leaf 6) (leaf -7))) (node 5 (leaf -6) (leaf 7)))
(test (negate (node 7 (node 5 (leaf 6) (leaf 7)) (leaf 2))) (node -7 (node -5 (leaf -6) (leaf -7)) (leaf -2)))

;; Part 3 — Contains?


(define (contains? [tree : Tree] [num : Number]) : Boolean
  (type-case Tree tree
    [(leaf n) (= n num)]
    [(node n l r) (or (= n num) (or (contains? l num) (contains? r num)))]))


(test (contains? (node 5 (leaf 6) (leaf 7)) 6) #t)
(test (contains? (node 5 (leaf 6) (leaf 7)) 4) #f)
(test (contains? (leaf 7) 6) #f)
(test (contains? (leaf 7) 7) #t)
(test (contains? (node 2 (node 4 (leaf 6) (leaf 8)) (node 10 (leaf 12) (leaf 14))) 8) #t)
(test (contains? (node 2 (node 4 (leaf 6) (leaf 8)) (node 10 (leaf 12) (leaf 14))) 7) #f)

;; Part 4 — Big Leaves?

(define (big-leaves? [tree : Tree]) : Boolean
  (type-case Tree tree
    [(leaf n) (bigger-leaves? tree 0)]
    [(node n l r) (bigger-leaves? tree 0)]))


(define (bigger-leaves? [tree : Tree] [num : Number]) : Boolean
  (type-case Tree tree
    [(leaf n) (> n num)]
    [(node n l r) (and (bigger-leaves? l (+ n num)) (bigger-leaves? r (+ n num)))]))


(test (big-leaves? (node 5 (leaf 6) (leaf 7))) #t)
(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) #f)
(test (big-leaves? (leaf 5)) #t)
(test (big-leaves? (node 8 (leaf 7) (leaf 7))) #f)
(test (big-leaves? (leaf 0)) #f)
(test (big-leaves? (leaf -1)) #f)
(test (big-leaves? (node -1 (leaf 1) (leaf 2))) #t)

(test (bigger-leaves? (leaf 6) 5) #t)
(test (bigger-leaves? (leaf 4) 5) #f)
(test (bigger-leaves? (node 5 (leaf 10) (leaf 5)) 5) #f)
(test (bigger-leaves? (node 5 (leaf 10) (leaf 10)) 4) #t)



