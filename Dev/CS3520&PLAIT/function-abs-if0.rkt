#lang plait

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (appE [s : Symbol]
        [arg : Exp])
  (abs-val [ex : Exp])
  (if-zeroE [ex : Exp]
            [thn : Exp]
            [els : Exp])
  (letE [n : Symbol] [rhs : Exp]
        [body : Exp]))

(define-type Func-Defn
  (fd [name : Symbol] 
      [arg : Symbol] 
      [body : Exp]))



(module+ test
  (print-only-errors #t))

;; An EXP is either
;; - `NUMBER
;; - `SYMBOL
;; - `{+ EXP EXP}
;; - `{* EXP EXP}
;; - `{SYMBOL EXP)

;; A FUNC-DEFN is
;; - `{define {SYMBOL SYMBOL} EXP}

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{abs ANY} s)
     (abs-val (parse (second (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY} s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (if-zeroE (parse (second (s-exp->list s)))
               (parse (third (s-exp->list s)))
               (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [else (error 'parse "invalid input")]))

(define (parse-fundef [s : S-Exp]) : Func-Defn
  (cond
    [(s-exp-match? `{define {SYMBOL SYMBOL} ANY} s)
     (fd (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
         (s-exp->symbol (second (s-exp->list (second (s-exp->list s)))))
         (parse (third (s-exp->list s))))]
    [else (error 'parse-fundef "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{double 9})
        (appE 'double (numE 9)))
  (test (parse `{abs -1})
        (abs-val (numE -1)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")

  (test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double 'x (plusE (idE 'x) (idE 'x))))
  (test/exn (parse-fundef `{def {f x} x})
            "invalid input")

  (define double-def
    (parse-fundef `{define {double x} {+ x x}}))
  (define quadruple-def
    (parse-fundef `{define {quadruple x} {double {double x}}})))

;; interp ----------------------------------------
(define (interp [a : Exp] [defs : (Listof Func-Defn)]) : Number
  (type-case Exp a
    [(numE n) n]
    [(idE s) (error 'interp "free variable")]
    [(plusE l r) (+ (interp l defs) (interp r defs))]
    [(multE l r) (* (interp l defs) (interp r defs))]
    [(appE s arg) (local [(define fd (get-fundef s defs))]
                    (interp (subst (numE (interp arg defs))
                                   (fd-arg fd)
                                   (fd-body fd))
                            defs))]
    [(abs-val s) (local [(define result (interp s defs))]
                   (if (< result 0)
                       (* -1 result)
                       result))]
    [(if-zeroE ex thn els) (if (equal? 0 (interp ex defs))
                               (interp thn defs)
                               (interp els defs))]))

(module+ test
  (test (interp (parse `2) empty)
        2)
  (test/exn (interp (parse `x) empty)
            "free variable")
  (test (interp (parse `{+ 2 1}) empty)
        3)
  (test (interp (parse `{* 2 1}) empty)
        2)
  (test (interp (parse `{+ {* 2 3}
                           {+ 5 8}})
                empty)
        19)
  (test (interp (parse `{double 8})
                (list double-def))
        16)
  (test (interp (parse `{quadruple 8})
                (list double-def quadruple-def))
        32)
  (test (interp (parse `{abs -1})
                empty)
        1)

  (test (interp (parse `{abs {f 3}})
                (list (parse-fundef `{define {f x} {+ x 1}})))
        4)

  (test (interp (parse `{f 3})
                (list (parse-fundef `{define {f x} {+ {abs x} 1}})))
        4)
  (test (interp (parse `{if0 0 1 2})
                empty)
        1)
  (test (interp (parse `{if0 1 1 2})
                empty)
        2)
  (test (interp (parse `{f 3})
                (list (parse-fundef `{define {f x} {if0 x 2 x}})))
        3))

;; get-fundef ----------------------------------------
(define (get-fundef [s : Symbol] [defs : (Listof Func-Defn)]) : Func-Defn
  (type-case (Listof Func-Defn) defs
    [empty (error 'get-fundef "undefined function")]
    [(cons def rst-defs) (if (eq? s (fd-name def))
                             def
                             (get-fundef s rst-defs))]))

(module+ test
  (test (get-fundef 'double (list double-def))
        double-def)
  (test (get-fundef 'double (list double-def quadruple-def))
        double-def)
  (test (get-fundef 'double (list quadruple-def double-def))
        double-def)
  (test (get-fundef 'quadruple (list quadruple-def double-def))
        quadruple-def)
  (test/exn (get-fundef 'double empty)
            "undefined function"))

;; subst ----------------------------------------
(define (subst [what : Exp] [for : Symbol] [in : Exp])
  (type-case Exp in
    [(numE n) in]
    [(idE s) (if (eq? for s)
                 what
                 in)]
    [(plusE l r) (plusE (subst what for l)
                        (subst what for r))]
    [(multE l r) (multE (subst what for l)
                        (subst what for r))]
    [(appE s arg) (appE s (subst what for arg))]
    [(abs-val s)  (abs-val (subst what for s))]
    [(if-zeroE ex thn els)
     (if-zeroE (subst what for ex)
               (subst what for thn)
               (subst what for els))]
    [(letE n rhs body)
     (letE n
           (subst what for rhs)
           (if (symbol=? n for)
               body
               (subst what for body)))]))

(module+ test
  (test (subst (numE 8) 'x (numE 9))
        (numE 9))
  (test (subst (numE 8) 'x (idE 'x))
        (numE 8))
  (test (subst (numE 8) 'x (idE 'y))
        (idE 'y))
  (test (subst (numE 8) 'x (plusE (idE 'x) (idE 'y)))
        (plusE (numE 8) (idE 'y)))
  (test (subst (numE 8) 'x (multE (idE 'y) (idE 'x)))
        (multE (idE 'y) (numE 8)))
  (test (subst (numE 8) 'x (appE 'double (idE 'x)))
        (appE 'double (numE 8))))

(module+ test
  (test (subst (parse `8) 'x (parse `9))
        (numE 9))
  (test (subst (parse `8) 'x (parse `x))
        (numE 8))
  (test (subst (parse `8) 'x (parse `y))
        (idE 'y))
  (test (subst (parse `8) 'x (parse `{+ x y}))
        (parse `{+ 8 y}))
  (test (subst (parse `8) 'x (parse `{* y x}))
        (parse `{* y 8}))
  (test (subst (parse `8) 'x (parse `{double x}))
        (parse `{double 8}))
  (test (subst (parse `1) 'x (parse `{abs x}))
        (parse `{abs 1})))
