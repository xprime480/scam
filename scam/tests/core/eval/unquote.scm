(import (lib test narc))

(narc-label "Unquote")

(define x 99)

(narc-expect
 ('(unquote (+ 2 2)) ',(+ 2 2))
 (4                  `,(+ 2 2))
 ('(+ (+ 3 (+ 99 1 2 3)) 2)
  `(+ (+ 3 (+ ,x 1 2 3)) 2)))

(narc-report)
