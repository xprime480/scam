(import (lib test narc))

(narc-label "Make some strings")

(narc-expect
 (""     (make-string 0))
 (""     (make-string 0 #\x))
 ("   "  (make-string 3))
 ("ii"   (make-string 2 #\i))

 (""     (string))
 ("1x*"  (string #\1 #\x #\*)))

(narc-catch
 (:args (make-string -5 #\*))
 (:args (make-string 335 3))
 (:args (string 1 2 3)))

(narc-report)
