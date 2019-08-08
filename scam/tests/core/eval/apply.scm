;;; Simple Apply
;;;

(narc-label "Apply")

(load "lib/test/test-handler.scm")

(narc-expect
 (99 (apply * (list 3 33)))
 (13 (apply (lambda (x y) (+ x 2 (* 2 y))) '(1 5)))
 (:args (test-err-cat (apply)))
 (:args (test-err-cat (apply * 3 33))))

(narc-report)
