(import (only (scam class)
              make-class)
        (test narc))

(narc-label "Equality")

(define Class1 (make-class
                Root
                ()
                (init ())))

(define Class2 (make-class
                Root
                (x)
                (init (q c) (set! x (+ 3 q c)))))

(define Class3 (make-class
                Root
                (x)
                (init (q c) (set! x (+ 3 q c)))))

(define Class4 Class1)

(define thing1 (Class2 2 5))
(define thing2 (Class2 -1 33))

(narc-expect
 (#t (equal? Class1 Class1))
 (#f (equal? Class1 Class2))
 (#f (equal? Class1 Class3))            ; even with same definition
 (#t (equal? Class1 Class4))

 (#t (equal? thing1 thing1))
 (#f (equal? thing1 thing2)))

(narc-report)
