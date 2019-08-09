;;; Read operations
;;;

(narc-label "Read operations on strings")

(define test "hello, world")

(narc-expect
 (0   (string-length ""))
 (12  (string-length test))

 (#\h (string-ref test 0))
 (#\o (string-ref test 4)))

(narc-catch
 (:args (string-length :i-am-no-string))

 (:args (string-ref "" 0))
 (:args (string-ref test -1))
 (:args (string-ref test 99)))

;;(string-ref test 6) ;; want a better representation of a space

(narc-report)
