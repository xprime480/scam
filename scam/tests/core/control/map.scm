(import (only (scheme base) integer? list map)
        (test narc))

(narc-label "Map")

(narc-expect
 ('(#t #f #f) (map integer? (list 1 0.123 "xx"))))

(narc-report)
