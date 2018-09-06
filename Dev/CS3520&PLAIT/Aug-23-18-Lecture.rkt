#lang plait

;;(print-only-errors #t)

(define-type Robot-instr
  (forward [feet : Number ])
  (left)
  (right))

(define (amount-forward [instr : Robot-instr])
  (type-case Robot-instr instr
    [(forward f) f ]
    [(left) 0]
    [(right) 0]))



(test (amount-forward (forward 10))
      '10)

(test (amount-forward (right))
      '0)

(test (amount-forward (left))
      '0)


(define (turn-or-not [instr : Robot-instr])
  (type-case Robot-instr instr
    [(forward f) #f ]
    [(left) #t ]
    [(right) #t ]))


(test (turn-or-not (forward 10))
      #f)
(test (turn-or-not (left))
      #t)
(test (turn-or-not (right))
      #t)

;; -------------------------
(define (total-distance [ instrs : (Listof Robot-instr)])
  (type-case (Listof Robot-instr) instrs
    [empty 0]
    [(cons fst rst) (+ (amount-forward fst) (total-distance rst))]))


(test (total-distance empty)
      0)

(test (total-distance (cons (forward 4) (cons (left) empty)))
      4)



(define (is-turn? [ instrs : (Listof Robot-instr)]
                  [prev : Robot-instr])
  (type-case (Listof Robot-instr) instrs
    [empty #f]
    [(cons fst rst) (or (turn-or-not fst)
                        (is-turn? rst))]))

(test (is-turn? (cons (forward 5) (cons (left) empty)))
      #t)
(test (is-turn? empty)
      #f)
(test (is-turn? (cons (forward 6) (cons (forward 3) empty)))
      #f)


;; ---------- U Turns >>><<<<<




(test (any-u-turns? (cons (forward 5) (cons (left) empty))
                    (right))
      #f)
(test (any-u-turns? empty)
      #f)
(test (any-u-turns? (cons (forward 6) (cons (forward 3) empty))
                    (right))
      #f)
(test (any-u-turns? (cons (left) (cons (left) empty)))
      #t)
(test (any-u-turns? (cons (right) (cons (right) empty)))
      #t)
(test (any-u-turns? (cons (right) (cons (left) empty)))
      #f)
