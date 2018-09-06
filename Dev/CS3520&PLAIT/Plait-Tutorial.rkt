#lang plait



(define (is-odd? x)
  (if (zero? x)
      #f
      (is-even? (- x 1))))


(define (is-even? x)
  (if (zero? x)
      #t
      (is-odd? (- x 1))))

(define groceries : (Listof String) '("milk" "cookies"))

(define (starts-milk? [items : (Listof String)]) : Boolean
  (equal? (first items) "milk"))

(local [(define pi-ish 3)
        (define (approx-circle-area r)
          (* pi-ish (* r r)))]
  (approx-circle-area 2))


(define-type Animal
  (tiger [color : Symbol]
         [stripe-count : Number])
  (snake [color : Symbol]
         [weight : Number]
         [food : String]))

(define (animal-color [a : Animal]) : Symbol
  (cond
    [(tiger? a) (tiger-color  a)]
    [(snake? a) (snake-color a)]))

(define (got-milk? [items : (Listof String)])
  (type-case (Listof String) items
    [empty #f]
    [(cons item rst-items) ....]))

(trace got-milk?)

(define-type Grade
  (letter [alpha : Symbol])
  (pass-fail [pass? : Boolean])
  (incomplete))

(define (taste s)
    (cond
      [(equal? s "milk") 'good]
      [else 'not-as-good]))

(module+ test
  (test (retaste "milk") '(still good)))

(define (retaste s)
  (list 'still (taste s)))