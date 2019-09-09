(import (only (scheme base) *)
        (only (scam class) make-class)
        (test narc))

(narc-label "Class Member")

(define Trivial
  (make-class
   Root
   ()
   (get () (self jarthur 1))
   (jarthur (n) (* n 17))))

(define obj (Trivial))

(narc-expect
 (17 (obj get))
 (51 (obj jarthur 3)))

(narc-report)
