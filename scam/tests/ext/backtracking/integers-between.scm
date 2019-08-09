;;; Generate integers between lower and upper bounds.
;;;

(narc-label "Integers Between")

(load "lib/prelude.scm")
(load "lib/numeric.scm")

(define v (lambda ()
            (integers-between 1 3)))
(define f (lambda ()
            (backtrack)
            v))

(narc-expect
 (1 1))

(narc-skip
 (1 *v*)
 (2 (f))
 (#t (begin
       (display "hi")
       (newline)
       #t))
 (3 (f))
 ("No more choices" ?))

(narc-report)
