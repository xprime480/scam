
(load "scripts/equality/differentclass.scm")

(define thing1 (Other 2 5))
(define thing2 (Other -1 33))

(equal? thing1 thing2)
