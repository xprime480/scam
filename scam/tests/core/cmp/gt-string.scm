(import (only (scheme base) >)
        (test narc))

(narc-label "CMP > String")

(narc-expect
 (#t (> "a"))
 (#f (> "a" "a"))
 (#t (> "b" "a"))

 (#t (> "zz" "z" "w" "aaaazzz"))
 (#f (> "z" "xa" "ez" "q")))

(narc-report)

