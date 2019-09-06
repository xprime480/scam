(import (test narc))

(narc-label "Append string")

(narc-expect
 (""              (string-append))
 (""              (string-append ""))
 ("Hello"         (string-append "Hello"))
 ("Hello, World"  (string-append "Hello" ", World"))
 ("abc"           (string-append "a" "b" "c")))

(narc-catch
 (:args (string-append 1 2 3)))

(narc-report)
