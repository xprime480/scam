;;; Test string-append
;;;

(narc-label "Append string")

(load "lib/test/test-handler.scm")

(narc-expect
 (""              (string-append))
 (""              (string-append ""))
 ("Hello"         (string-append "Hello"))
 ("Hello, World"  (string-append "Hello" ", World"))
 ("abc"           (string-append "a" "b" "c"))
 (:args           (test-err-cat (string-append 1 2 3))))

(narc-report)
