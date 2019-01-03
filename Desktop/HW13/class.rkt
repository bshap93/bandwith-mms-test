#lang plait

(define-type Exp
  (numE [n : Number])
  (plusE [lhs : Exp]
         [rhs : Exp])
  (multE [lhs : Exp]
         [rhs : Exp])
  (argE)
  (thisE)
  (newE [class-name : Symbol]
        [args : (Listof Exp)])
  (getE [obj-expr : Exp]
        [field-name : Symbol])
  (castE [class-name : Symbol]
         [exp : Exp])
  (if0E [cnd : Exp]
        [thn : Exp]
        [els :  Exp])
  (sendE [obj-expr : Exp]
         [method-name : Symbol]
         [arg-expr : Exp])
  (ssendE [obj-expr : Exp]
          [class-name : Symbol]
          [method-name : Symbol]
          [arg-expr : Exp])
  (nullE))

(define-type Class
  (classC [super : Symbol]
          [field-names : (Listof Symbol)]
          [methods : (Listof (Symbol * Exp))]))

(define (get-field-names [class-name : Symbol]
                         [classes : (Listof (Symbol * Class))])
  (let ([theclass (find classes class-name)])
    (type-case Class theclass
      [(classC super field-names methods) field-names])))

(define-type Value
  (numV [n : Number])
  (objV [class-name : Symbol]
        [field-values : (Listof Value)])
  (nullV))

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (find [l : (Listof (Symbol * 'a))] [name : Symbol]) : 'a
  (type-case (Listof (Symbol * 'a)) l
    [empty
     (error 'find (string-append "not found: " (symbol->string name)))]
    [(cons p rst-l)
     (if (symbol=? (fst p) name)
         (snd p)
         (find rst-l name))]))

(module+ test
  (test (find (list (values 'a 1)) 'a)
        1)
  (test (find (list (values 'a 1) (values 'b 2)) 'b)
        2)
  (test/exn (find empty 'a)
            "not found: a")
  (test/exn (find (list (values 'a 1)) 'x)
            "not found: x"))

;; ----------------------------------------

(define interp : (Exp (Listof (Symbol * Class)) Value Value -> Value)
  (lambda (a classes this-val arg-val)
    (local [(define (recur expr)
              (interp expr classes this-val arg-val))]
      (type-case Exp a
        [(numE n) (numV n)]
        [(plusE l r) (num+ (recur l) (recur r))]
        [(multE l r) (num* (recur l) (recur r))]
        [(thisE) this-val]
        [(argE) arg-val]
        [(newE class-name field-exprs)
         (local [(define c (find classes class-name))
                 (define vals (map recur field-exprs))]
           (if (= (length vals) (length (classC-field-names c)))
               (objV class-name vals)
               (error 'interp "wrong field count")))]
        [(getE obj-expr field-name)
         (type-case Value (recur obj-expr)
           [(objV class-name field-vals)
            (type-case Class (find classes class-name)
              [(classC super field-names methods)
               (find (map2 (lambda (n v) (values n v))
                           field-names
                           field-vals)
                     field-name)])]
           [else (error 'interp "not an object")])]
        [(sendE obj-expr method-name arg-expr)
         (local [(define obj (recur obj-expr))
                 (define arg-val (recur arg-expr))]
           (type-case Value obj
             [(objV class-name field-vals)
              (call-method class-name method-name classes
                           obj arg-val)]
             [else (error 'interp "not an object")]))]
        [(ssendE obj-expr class-name method-name arg-expr)
         (local [(define obj (recur obj-expr))
                 (define arg-val (recur arg-expr))]
           (call-method class-name method-name classes
                        obj arg-val))]
        ;; -----------------#2 Change---------------------
        [(castE class-name exp)
         (let ([val (recur exp)])
           (type-case Value val
             [(objV class-name2 field-values)
              (cond
                [(instance-of? class-name2 class-name classes)
                 val]
                [else (error 'interp "not an instance")])]
             [else (error 'interp "not an object")]))]
        ;; -----------------#3 Change---------------------
        [(if0E cnd thn els)
         (let ([num (recur cnd)])
           (type-case Value num
             [(numV n)
              (cond
                [(= 0 n) (recur thn)]
                [else (recur els)])]
             [else (error 'interp "not a number")]))]
        ;; -----------------#4 Change---------------------
        [(nullE) (nullV)]))))

;; -----------------#2 Change---------------------
;(define (fields-of-parent [field-values : (Listof Value)]
;                          [parent-name : Symbol]
;                          [child-name : Symbol]
;                          [classes : (Listof (Symbol * Class))])
;  (let ([parent-fields (get-field-names parent-name classes)])
;    (let ([child-fields (get-field-names child-name classes)])
;      (cond
;        [(member (first child-fields) parent-fields)
;         (cons (first field-values) (fields-of-parent (rest field-values) parent-name child-name classes))]
;        [else (fields-of-parent (rest field-values) parent-name child-name classes)]))))
                          
;; -----------------#2 Change---------------------        
(define (instance-of? [class-name : Symbol]
                      [parent-name : Symbol]
                      [classes : (Listof (Symbol * Class))])
  (cond
    [(equal? class-name parent-name) #t]
    [else
     (type-case Class (find classes class-name)
       [(classC super field-names methods)
        (instance-of? super parent-name classes)])]))
           
(define (call-method class-name method-name classes
                     obj arg-val)
  (type-case Class (find classes class-name)
    [(classC super field-names methods)
     (let ([body-expr (find methods method-name)])
       (interp body-expr
               classes
               obj
               arg-val))]))

(define (num-op [op : (Number Number -> Number)]
                [op-name : Symbol] 
                [x : Value]
                [y : Value]) : Value
  (cond
    [(and (numV? x) (numV? y))
     (numV (op (numV-n x) (numV-n y)))]
    [else (error 'interp "not a number")]))

(define (num+ x y) (num-op + '+ x y))
(define (num* x y) (num-op * '* x y))

;; ----------------------------------------
;; Examples

(module+ test
  (define posn-class 
    (values 'Posn
            (classC
             'Object
             (list 'x 'y)
             (list (values 'mdist
                           (plusE (getE (thisE) 'x) (getE (thisE) 'y)))
                   (values 'addDist
                           (plusE (sendE (thisE) 'mdist (numE 0))
                                  (sendE (argE) 'mdist (numE 0))))
                   (values 'addX
                           (plusE (getE (thisE) 'x) (argE)))
                   (values 'multY (multE (argE) (getE (thisE) 'y)))
                   (values 'factory12 (newE 'Posn (list (numE 1) (numE 2))))))))
    
  (define posn3D-class
    (values 'Posn3D
            (classC
             'Posn
             (list 'x 'y 'z)
             (list (values 'mdist (plusE (getE (thisE) 'z)
                                         (ssendE (thisE) 'Posn 'mdist (argE))))
                   (values 'addDist (ssendE (thisE) 'Posn 'addDist (argE)))))))

  (define posn27 (newE 'Posn (list (numE 2) (numE 7))))
  (define posn531 (newE 'Posn3D (list (numE 5) (numE 3) (numE 1))))
  (define posn828 (newE 'Posn (list (numE 8) (numE 28))))
  (define posn42432 (newE 'Posn3D (list (numE 4) (numE 24) (numE 32))))

  (define (interp-posn a)
    (interp a (list posn-class posn3D-class) (numV -1) (numV -1))))

;; ----------------------------------------

(module+ test
  (test (interp (numE 10) 
                empty (objV 'Object empty) (numV 0))
        (numV 10))
  ;; -----------------#4 Change---------------------
  (test (interp (nullE) 
                empty (objV 'Object empty) (numV 0))
        (nullV))
  ;; -----------------#3 Change---------------------

  (test (interp (if0E (numE 0) posn828 posn27)
                (list posn-class posn3D-class) (objV 'Object empty) (numV 0))
        (interp-posn posn828))
  (test (interp (if0E (numE 1) posn828 posn27)
                (list posn-class posn3D-class) (objV 'Object empty) (numV 0))
        (interp-posn posn27))
  (test/exn (interp (if0E posn828 posn828 posn27)
                 (list posn-class posn3D-class) (objV 'Object empty) (numV 0))
         "not a number")
        
  ;; -----------------#2 Change---------------------

  (test (interp-posn (castE 'Posn posn531))
        (objV 'Posn3D (list (numV 5) (numV 3) (numV 1))))
  (test (interp-posn (castE 'Posn posn27))
        (objV 'Posn (list (numV 2) (numV 7))))
  (test/exn (interp-posn (castE 'Posn3D posn27))
              "not")
  (test/exn (interp-posn (castE 'Number (numE 3)))
            "not")


  (test (interp (plusE (numE 10) (numE 17))
                empty (objV 'Object empty) (numV 0))
        (numV 27))
  (test (interp (multE (numE 10) (numE 7))
                empty (objV 'Object empty) (numV 0))
        (numV 70))
  ;;------------#5 Change -----------------
  (test (interp-posn (nullE))
        (nullV))

  (test (interp-posn (newE 'Posn (list (numE 2) (numE 7))))
        (objV 'Posn (list (numV 2) (numV 7))))

  (test (interp-posn (sendE posn27 'mdist (numE 0)))
        (numV 9))
  
  (test (interp-posn (sendE posn27 'addX (numE 10)))
        (numV 12))

  (test (interp-posn (sendE (ssendE posn27 'Posn 'factory12 (numE 0))
                            'multY
                            (numE 15)))
        (numV 30))

  (test (interp-posn (sendE posn531 'addDist posn27))
        (numV 18))
  
  (test/exn (interp-posn (plusE (numE 1) posn27))
            "not a number")
  (test/exn (interp-posn (getE (numE 1) 'x))
            "not an object")
  (test/exn (interp-posn (sendE (numE 1) 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (ssendE (numE 1) 'Posn 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (newE 'Posn (list (numE 0))))
            "wrong field count"))
