;;; Test creation of pair values
;;;

(narc-label "Create Pairs")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args     (test-err-cat (cons)))
 (:args     (test-err-cat (cons 1)))
 (:args     (test-err-cat (cons 1 2 3)))
 ('(1 . 2)  (cons 1 2))
 ('(1)      (cons 1 '()))
 ('()       (list))
 ('(1 2 3)  (list 1 2 3)))

(narc-report)
