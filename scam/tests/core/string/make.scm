;;; Make some strings
;;;

(narc-label "Make some strings")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args  (test-err-cat (make-string -5 #\*)))
 (:args  (test-err-cat (make-string 335 3)))

 (""     (make-string 0))
 (""     (make-string 0 #\x))
 ("   "  (make-string 3))
 ("ii"   (make-string 2 #\i))

 (:args  (test-err-cat (string 1 2 3)))

 (""     (string))
 ("1x*"  (string #\1 #\x #\*)))

(narc-report)
