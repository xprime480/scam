;;; Read bad input or bad syntax
;;;

(narc-label "Read Errors")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (read)))
 (:args (test-err-cat (read (open-input-string "") :nope)))
 (:args (test-err-cat (read 2)))
 (:args (test-err-cat (read (open-input-string "(2 3 4"))))
 (:args (test-err-cat (read (open-input-string ")")))))

(narc-report)
