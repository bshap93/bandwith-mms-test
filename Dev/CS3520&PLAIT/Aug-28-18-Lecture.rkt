#lang plait

(print-only-errors #t)

(define-type Exp
  (numE [n : Number])
  (plusE [l : (Listof Exp)])
  (multE [l : Exp]
         [r : Exp])
  (diviE [l : Exp]
         [r : Exp])
  (absE [n : Exp]))

;; An EXP-S-EXP is either
;; - `NUMBER
;; - `{+ EXP-S-EXP EXP-S-EXP}
;; - `{* EXP-S-EXP EXP-S-EXP}

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `{+ ANY ...} s)
     (plusE (map parse (rest (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{/ ANY ANY} s)
     (diviE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{abs ANY} s)
     (absE (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(test (parse `2)
      (numE 2))
(test (parse `{+ 2 1})
      (plusE (list (numE 2) (numE 1))))
(test (parse `{* 3 4})
      (multE (numE 3) (numE 4)))
(test (parse `{+ {* 3 4} 8})
      (plusE (list (multE (numE 3) (numE 4))
             (numE 8))))
(test (parse `{/ 6 2})
      (diviE (numE 6) (numE 2)))
(test/exn (parse `{/ 62})
      "invalid input")
(test (parse `{abs 2})
      (absE (numE 2)))
(test (parse `{+ 2 1 6 3})
      (plusE (list (numE 2) (numE 1) (numE 6) (numE 3))))

(define (interp [a : Exp]) : Number
  (type-case Exp a
    [(numE n) n]
    [(plusE l) (sum-list l)]
    [(multE l r) (* (interp l) (interp r))]
    [(diviE l r) (/ (interp l) (interp r))]
    [(absE n)
     (local [(define v (interp n))]
       (cond
         [(< 0 v) v]
         [(>= 0 v) (- 0 v)]))]))

(define (sum-list [l : (Listof Exp)])
  (type-case (Listof Exp) l
    [empty (numE 0)]
    [(cons f r) (+ (interp f) (sum-list r))]))

(test (sum-list (list))
      0)
(test (sum-list (list (numE 1) (numE 2)))
      3)


(test (interp (parse `2))
      2)
(test (interp (parse `{+ 2 1}))
      3)
(test (interp (parse `{* 2 1}))
      2)
(test (interp (parse `{+ {* 2 3}
                         {+ 5 8}}))
      19)
(test (interp (parse `{/ 6 3}))
      2)
(test (interp (parse `{abs -3}))
      3)
(test (interp (parse `{abs 3}))
      3)
(test (interp (parse `{abs {+ 5 -22}}))
      17)
(test (interp (parse `{+}))
      0)
(test (interp (parse `{+ 33}))
      33)
(test (interp (parse `{+ 1 2 3 4}))
      10)
