;;; Formal parameters mask symbols of the same name in
;;; defining environment.
;;;

(narc-label "Lambda Formals Override Env")

(define x 0.0)
(define (f x)
  (/ 1.0 x))

(narc-expect
 (#i1/2 (f 2)))

(narc-report)
