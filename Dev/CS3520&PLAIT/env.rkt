#lang plait

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [arg : (Listof Exp)])
  (multE [l : Exp]
         [r : Exp])
  (appE [s : Symbol]
        [arg : Exp])
  (letE [n : Symbol] 
        [rhs : Exp]
        [body : Exp])
  (letQ [n : Exp]
        [body : Exp]))



(define-type Func-Defn
  (fd [name : Symbol] 
      [arg : Symbol] 
      [body : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Number]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ...} s)
     (plusE (parse-list (rest (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY} s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [(s-exp-match? '{let-q ANY ANY} s)
     (letQ (parse (second (s-exp->list s)))
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [else (error 'parse "invalid input")]))

(define (parse-list [i : (Listof S-Exp)]) : (Listof Exp)
  (type-case (Listof S-Exp) i
    [empty empty]
    [(cons f r) (cons (parse f) (parse-list r))]))

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
  (test (parse `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (list (numE 2) (numE 1))))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{double 9})
        (appE 'double (numE 9)))
  (test (parse`{+ 1 2 3})
      (plusE (list (numE 1) (numE 2) (numE 3))))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (letE 'x (plusE (list (numE 1) (numE 2)))
              (idE 'y)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")

  (test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double 'x (plusE (list (idE 'x) (idE 'x)))))
  (test/exn (parse-fundef `{def {f x} x})
            "invalid input")

  (test (parse-list empty)
        empty)

  (test (parse-list (list `1))
        (list (numE 1)))
  
  (define double-def
    (parse-fundef `{define {double x} {+ x x}}))
  (define quadruple-def
    (parse-fundef `{define {quadruple x} {double {double x}}})))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env] [fds : (Listof Func-Defn)]) : Number
  (type-case Exp a
    [(numE n) n]
    [(idE s) (lookup s env)]
    [(plusE lst) (interp-list lst env fds)]
    [(multE l r) (* (interp l env fds) (interp r env fds))]
    [(appE s arg) (local [(define fd (get-fundef s fds))]
                    (interp (fd-body fd)
                            (extend-env
                             (bind (fd-arg fd)
                                   (interp arg env fds))
                             mt-env)
                            fds))]
    [(letE n rhs body)
     (interp body
             (extend-env 
              (bind n (interp rhs env fds))
              env)
             fds)]))

(define (interp-list [i : (Listof Exp)] [env : Env] [fds : (Listof Func-Defn)]) : Number
  (type-case (Listof Exp) i
    [empty 0]
    [(cons f r) (+ (interp f env fds) (interp-list r env fds))]))

(module+ test
  (test (interp (parse `2) mt-env empty)
        2)
  (test/exn (interp (parse `x) mt-env empty)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x 9) mt-env)
                empty)
        9)
  (test (interp (parse `{+ 2 1}) mt-env empty)
        3)
  (test (interp (parse `{* 2 1}) mt-env empty)
        2)
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env
                empty)
        19)
  (test (interp (parse `{double 8})
                mt-env
                (list double-def))
        16)
  (test (interp (parse `{quadruple 8})
                mt-env
                (list double-def quadruple-def))
        32)
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env
                empty)
        10)
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                empty)
        12)
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                empty)
        5)
  (test/exn (interp (parse `{let {[y 5]}
                              {bad 2}})
                    mt-env
                    (list (parse-fundef `{define {bad x} {+ x y}})))
            "free variable"))

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

;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env]) : Number
  (type-case (Listof Binding) env
   [empty (error 'lookup "free variable")]
   [(cons b rst-env) (cond
                       [(symbol=? n (bind-name b))
                        (bind-val b)]
                       [else (lookup n rst-env)])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x 8) mt-env))
        8)
  (test (lookup 'x (extend-env
                    (bind 'x 9)
                    (extend-env (bind 'x 8) mt-env)))
        9)
  (test (lookup 'y (extend-env
                    (bind 'x 9)
                    (extend-env (bind 'y 8) mt-env)))
        8))
  
