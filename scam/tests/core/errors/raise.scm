;;; Raise errors of different objects
;;;

(narc-label "Raise")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (raise)))
 (:args (test-err-cat (raise 1 2 3)))
 ("bagels are not doughnuts" (raise "bagels are not doughnuts"))
 (2 (raise 2)))

(narc-report)
