#lang plait

(define-type GUI
  (label [text : String])
  (button [text : String]
          [enabled? : Boolean])
  (choice [items :  (Listof String)]
          [selected : Number])

  (vertical [top : GUI]
             [bottom : GUI])
  (horizontal [left : GUI]
              [right : GUI]))

(define (read-screen [g : GUI]) : (Listof String)
  (type-case GUI g
    [(label t) (list t)]
    [(button t e?) (list t)]
    [(choice i s) i]
    [(vertical t b) (append (read-screen t) (read-screen b))]
    [(horizontal l r) (append (read-screen l) (read-screen r))]
    ))

(define (enable-button [g : GUI] [name : String]) : GUI
  (type-case GUI g
    [(label t) g]
    [(button t e?) (if (string=? name t)
                       (button name #t)
                       g)]
    [(choice i s) g]
    [(vertical t b) (vertical (enable-button t name) (enable-button b name))]
    [(horizontal l r) (horizontal (enable-button l name) (enable-button r name))]))

(define (prefix [n : Number] [t : String])
  (string-append (to-string n)
                 (string-append " " t )))

(test (prefix 3 "Hi")
      "3 Hi")


(define (add1 [n : Number])
  (+ n 1))

(define (show-depth [g : GUI] [n : Number]) : GUI
  (type-case GUI g
    [(label t) (label (prefix n t))]
    [(button t e?) (button (prefix n t) e?)]
    [(choice i s) g]
    [(vertical t b) (vertical (show-depth t (add1 n)) (show-depth b (add1 n)))]
    [(horizontal l r) (horizontal (show-depth l (add1 n)) (show-depth r (add1 n)))]))


(test (show-depth (label "Pick a fruit:") 0)
      (label "0 Pick a fruit:"))
(test (show-depth (label "Hello") 1)
      (label "1 Hello"))
(test (show-depth (button "Ok" #f) 0)
      (button "0 Ok" #f))

(test (show-depth (choice (list "Yes" "No") 0) 0)
      (choice (list "Yes" "No") 0))
(test (show-depth (vertical (label "Hello")
                            (button "Ok" #t))
                  0)
      (vertical (label "1 Hello")
                (button "1 Ok" #t)))
(test (show-depth (horizontal (vertical (label "Yesterday")
                                        (label "Tomorrow"))
                              (button "Cancel" #f))
                  0) 
      (horizontal (vertical (label "2 Yesterday")
                            (label "2 Tomorrow"))
                  (button "1 Cancel" #f)))






(test (enable-button (label "Pick a fruit:") "Ok")
      (label "Pick a fruit:"))

(test (enable-button (button "Ok" #f) "Ok")
      (button "Ok" #t))
(test (enable-button (button "Ok" #t) "Ok")
      (button "Ok" #t))
(test (enable-button (button "Cancel" #t) "Ok")
      (button "Cancel" #t))
(test (enable-button (choice (list "Good" "Bad") 0) "Ok")
      (choice (list "Good" "Bad") 0))
(test (enable-button (vertical (label "Hello") (button "Ok" #f)) "Ok")
      (vertical (label "Hello") (button "Ok" #t)))
(test (enable-button (horizontal (button "Ok" #f) (button "Cacnel" #t)) "Ok")
      (horizontal (button "Ok" #t) (button "Cacnel" #t)))




(test (read-screen (label "Pick a fruit:"))
      (list "Pick a fruit:"))
(test (read-screen (button "Ok" #t))
      (list "Ok"))
(test (read-screen (choice (list "Apple" "Banana" "Coconut")
                           0))
      (list "Apple" "Banana" "Coconut"))

(test (read-screen (vertical (label "Pick a color:")
                             (choice (list "red" "green" "blue")
                                     0)))
      (list "Pick a color:" "red" "green" "blue"))

(test (read-screen (horizontal (button "Ok" #f)
                               (button "Cancel" #t)))
      (list "Ok" "Cancel"))

(define gui1
  (vertical
   (horizontal
    (label "Pick a fruit:")
    (choice
     (list "Apple" "Banana" "Coconut")
     0))
   (horizontal
    (button "Ok" #f)
    (button "Cancel" #t))))
(test (read-screen gui1)
      (list "Pick a fruit:" "Apple" "Banana" "Coconut" "Ok" "Cancel"))








  