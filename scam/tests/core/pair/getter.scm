;;; Test access to components of pair values
;;;

(narc-label "Car / Cdr")

(load "lib/test/test-handler.scm")

(define pair (cons 1 2))
(define not-a-pair 2)
(define list-val (list 1 2 3))

(narc-expect
 (:args (test-err-cat (car not-a-pair)))
 (:args (test-err-cat (cdr not-a-pair)))

 (1  (car pair))
 (2  (cdr pair))

 (1       (car list-val))
 ('(2 3)  (cdr list-val))
 (2       (car (cdr list-val))))

(narc-report)
