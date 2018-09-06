#lang plait

(print-only-errors #t)

(define-type Exp
  (numE [n : Number])
  (plusE [l : Exp]
         [r : Exp])
  (multE [l : Exp]
         [r : Exp]))

(define (interp [e : Exp]) : Number
  (type-case Exp e
    [(numE n) n]
    [(plusE l r) (+ (interp l) (interp r))]
    [(multE l r) (* (interp l) (interp r))]))

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test 

  (test (parse `2)
        (numE 2))
  (test (parse `{+ 1 2})
        (plusE (numE 1) (numE 2)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ 1 {* 3 4}})
        (plusE (numE 1)
               (multE (numE 3) (numE 4))))

  (test (interp (parse `2))
        2)

  (test (interp (parse `{+ 1 2}))
        3
        )

  (test (interp (parse `{* 3 4}))
        12)

  (test (interp (parse `{+ 1 {* 3 4}}))
        13)
  (test/exn (interp (parse `{- 2 3}))
            "invalid input")

  )