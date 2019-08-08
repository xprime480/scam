;;; Class with runtime initialization error
;;;

(narc-label "Class Init Error")

(load "lib/test/test-handler.scm")

(define Trivial (make-class
                 Root
                 (n)
                 (init (v) (set! n v))))
(narc-expect
 (:args (test-err-cat (Trivial (/ 1 0)))))

(narc-report)
