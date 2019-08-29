(import (lib test narc))

(narc-label "Quasiquote")

(narc-expect
 ('(+ 2 2) (quasiquote (+ 2 2)))
 ('(+ 2 2) `(+ 2 2)))

(narc-report)
