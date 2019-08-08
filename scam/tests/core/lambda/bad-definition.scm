;;; Malformed lambda expressions
;;;

(narc-label "Malformed Lambda Expressions")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (lambda)))
 (:args (test-err-cat (lambda 2 (+ 2 2))))
 (:args (test-err-cat (lambda (:keyword) (+ 2 2))))
 (:args (test-err-cat (lambda (x x) (* x 2))))
 (:args (test-err-cat (lambda (x . x) (* x 2))))
 (:args (test-err-cat (lambda (ok . :keyword) (+ 2 2)))))

(narc-report)
