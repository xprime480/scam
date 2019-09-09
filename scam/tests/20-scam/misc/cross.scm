(import (only (scheme base) + cons)
        (only (scam misc) cross)
        (test narc))

(narc-label "Cross")

(narc-expect
 ('() (cross cons '() '()))
 ('() (cross cons '() '((1) (2))))
 ('() (cross (lambda (a b) (+ a b)) '(1 2 3) '()))

 ('(2 3 4 3 4 5 4 5 6)
  (let ((a '(1 2 3)))
    (cross + a a))))

(narc-report)
