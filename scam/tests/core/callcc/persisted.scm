(import (test narc))

(narc-label "Persisted Continuation")

(define **cont** ())
(define x 0)

(set! x (+ 1 (call/cc
              (lambda (k)
                (set! **cont** k)
                99))))

(narc-expect (100 x))

(+ 2 3 (* 4 (/ 1 (**cont** 0)) (/ 2 0)))

(narc-expect (1 x))

(narc-report)

