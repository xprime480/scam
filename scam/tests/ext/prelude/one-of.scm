;;; One-of function
;;;

(narc-label "One-Of")

(load "lib/prelude.scm")

(narc-skip
 ("No more choices" (one-of (list)))
 (2                 (one-of (list 2)))
 (2                 (one-of (list 2 8 22)))
 (8                 ?))

(narc-report)
