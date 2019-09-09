(import (only (scheme base) open-input-string port?)
        (scheme read)
        (test narc))

(narc-label "String Input Port")

(define data (open-input-string "(+ 1 1)"))

(narc-expect
 (#t       (port? data))
 ('(+ 1 1) (read data)))

(narc-report)
