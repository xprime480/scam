(import (only (scheme base) eof-object? open-input-string)
        (scheme read)
        (test narc))

(narc-label "String Port")

(define text "1 2 3 4")
(define data (open-input-string text))

(narc-expect
 (1  (read data))
 (2  (read data))
 (3  (read data))
 (4  (read data))
 (#t (eof-object? (read data))))

(narc-report)
