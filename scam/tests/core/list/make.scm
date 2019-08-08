;;; Make some lists
;;;

(narc-label "Make Lists")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args        (test-err-cat (make-list -5 #\*)))
 ('()          (make-list 0))
 ('()          (make-list 0 #\x))
 ('(() () ())  (make-list 3))
 ('(#\i #\i)   (make-list 2 #\i))
 ('(1 2 3)     (list 1 2 3))
 ('()          (list)))

(narc-report)
