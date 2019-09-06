(import (only (scheme base) map)
        (test narc))

(narc-label "Map")

(narc-expect
 ('(2 4 6) (map (lambda (x) (+ x 1))
                (list 1 3 5))))

(narc-report)
