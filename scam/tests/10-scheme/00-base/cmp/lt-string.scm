(import (only (scheme base) <)
        (test narc))

(narc-label "CMP < String")

(narc-expect
 (#t (< "a"))
 (#f (< "a" "a"))
 (#t (< "a" "b")))

(narc-report)

