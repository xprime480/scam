(import (test narc))

(narc-label "Boolean Literals")

(narc-expect
 (#t #t)
 (#f #f)
 (#t #T)
 (#f #F)
 (#t #true)
 (#f #false))

(narc-report)
