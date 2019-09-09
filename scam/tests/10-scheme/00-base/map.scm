(import (only (scheme base) integer? list map)
        (test narc))

(narc-label "Map")

(narc-expect
 ('(#t #f #f) (map integer? (list 1 0.123 "xx")))
 ('(2 4 6) (map (lambda (x) (+ x 1))
                (list 1 3 5))))

(narc-report)
