;;; Class with nontrivial computation
;;;

(narc-label "Nontrivial Class Method")

(load "lib/prelude.scm")

(define Maxxer (make-class
                Root
                (val)
                (init (v) (set! val v))
                (get (n) (max val n))))

(define obj (Maxxer 0))

(narc-expect
 (0  (obj get -42))
 (42 (obj get (/ 84 2))))

(narc-report)