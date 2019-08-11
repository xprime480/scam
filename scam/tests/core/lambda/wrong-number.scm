;;; Call closure with the wrong number of parameters.
;;;

(narc-label "Lambda Wrong Arg Count")

(define f (lambda (x)
            (/ 1.0 x)))

(narc-catch
 (:args (f))
 (:args (f 1 2 3 4)))

(narc-report)