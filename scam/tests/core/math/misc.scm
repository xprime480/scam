(import (only (scheme inexact)
              nan?)
        (test narc))

(narc-label "Math Miscellaneous")

(narc-expect
 (16/5 (+ (* 2 3) (/ 1 5) (- 3)))
 (#t   (nan? (+ (* 2 3) (/ 1 (+ +nan.0 -5)) (- 3)))))

(narc-report)
