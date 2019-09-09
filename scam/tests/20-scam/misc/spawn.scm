(import (only (scheme base) + list)
        (only (scam misc) spawn)
        (test narc))

(narc-label "Spawn")

(define x 0)
(define y 0)

(if (spawn)
    (begin
      (set! x (+ x 1))
      (set! y (+ y 1)))
    (begin
      (set! y (+ y 1))))

(narc-expect
 ('(1 2) (list x y)))

(narc-report)
