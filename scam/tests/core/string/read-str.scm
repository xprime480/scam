;;; Read operations
;;;

(narc-label "Read operations on strings")

(load "lib/test/test-handler.scm")

(define test "hello, world")

(narc-expect
 (:args (test-err-cat (string-length :i-am-no-string)))
 (0     (string-length ""))
 (12    (string-length test))
 (:args (test-err-cat (string-ref "" 0)))
 (:args (test-err-cat (string-ref test -1)))
 (:args (test-err-cat (string-ref test 99)))
 (#\h   (string-ref test 0))
 (#\o   (string-ref test 4)))

;;(string-ref test 6) ;; want a better representation of a space

(narc-report)
