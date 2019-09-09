(import (only (scheme base) list make-list)
        (test narc))

(narc-label "Make Lists")

(narc-expect
 ('()          (make-list 0))
 ('()          (make-list 0 #\x))
 ('(() () ())  (make-list 3))
 ('(#\i #\i)   (make-list 2 #\i))
 ('(1 2 3)     (list 1 2 3))
 ('()          (list)))

(narc-catch
 (:args  (make-list -5 #\*)))

(narc-report)
