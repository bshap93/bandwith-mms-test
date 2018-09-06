#lang plait

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (appE [s : Symbol]
        [args : (Listof Exp)])
  (maxE [l : Exp]
        [r : Exp]))

(define-type Func-Defn
  (fd [name : Symbol] 
      [args : (Listof Symbol)] 
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
    [(s-exp-match? `{max ANY ANY} s)
     (maxE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ...} s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (parse-list (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-list [i : (Listof S-Exp)]) : (Listof Exp)
  (type-case (Listof S-Exp) i
    [empty empty]
    [(cons f r) (cons (parse f) (parse-list r))]))


  (test (parse-list (list `1))
        (list (numE 1)))
  (test (parse-list (list `{1 2 3}))
        (list (numE 1) (numE 2) (numE 3)))

(define (parse-fundef [s : S-Exp]) : Func-Defn
  (cond
    [(s-exp-match? `{define {SYMBOL ...} ANY} s)
     (cond
       ;[]
       [else (fd (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
         (map s-exp->symbol (rest (s-exp->list (second (s-exp->list s)))))
         (parse (third (s-exp->list s))))]
       )]

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
        (appE 'double (list(numE 9))))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")

  (test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double '{x} (plusE (idE 'x) (idE 'x))))
  (test/exn (parse-fundef `{def {f x} x})
            "invalid input")
  (test (interp (parse `{max 1 2})
                (list))
        2)
  (test (interp (parse `{max {+ 4 5} {+ 2 3}})
                (list))
        9)
  (test (interp (parse `{f 1 2})
                  (list (parse-fundef `{define {f x y} {+ x y}})))
        3)
  (test (interp (parse `{+ {f} {f}})
                (list (parse-fundef `{define {f} 5})))
        10)
  (test/exn (interp (parse `{f 1})
                    (list (parse-fundef `{define {f x y} {+ x y}})))
            "wrong arity")

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
    [(appE s args) (local [(define fd (get-fundef s defs))]
                    (interp (subst-list (map numE (interp-list args defs))
                                   (fd-args fd)
                                   (fd-body fd))
                            defs))]
    [(maxE l r) (max (interp l defs) (interp r defs))]))


(define (interp-list [as : (Listof Exp)] [defs : (Listof Func-Defn)]) : (Listof Number)
  (type-case (Listof Exp) as
    [empty empty]
    [(cons f r) (cons (interp f defs) (interp-list r defs))]))


(test (interp-list (list) (list))
      (list))
(test (interp-list (list (parse `2) (parse `{+ 1 6})) (list))
      (list (numE 2) (plusE (numE 1) (numE 6))))



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
        32))
  (test (interp (multE (maxE (numE 2) (numE 5)) (maxE (numE 2) (numE 5))) empty)
      25)

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
(define (subst [what : Exp] [for : Symbol] [in : Exp]) : Exp
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
    [(maxE l r) (maxE (subst what for l)
                      (subst what for r))]))

(define (subst-list [whats : (Listof Exp)] [fors : (Listof Symbol)] [in : Exp]) : Exp
  (type-case (Listof Symbol) fors
    [empty in]
    [(cons f rst) (cons (subst what f in) (subst-list what rst in))]))

(module+ test
  (test (subst-list '() '() (parse `9))
        (numE 9)) 
  (test (subst-list (list(parse `4)(parse `7)) '{x y} (parse `y))
        (numE 7))
  (test (subst (list (parse `8) (parse `{+ 8 8}) (parse `4)) '{x y z} (parse `y))
        (numE 16))
  (test (subst-list (list (parse `8)) '{x} (parse `x))
        (parse `8)))

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
        (parse `{double 8})))
  (test (subst (parse `8) 'x (parse `{max x x}))
        (parse `{max 8 8}))
  (test (subst (parse `{max 2 5}) 'x (parse `{* x x}))
        (parse `{* {max 2 5} {max 2 5}}))
