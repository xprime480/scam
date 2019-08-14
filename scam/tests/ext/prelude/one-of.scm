(import lib/prelude)

(narc-label "One-Of")

(narc-catch
 (:values (one-of (list))))

(narc-skip
 (2 (one-of (list 2)))
 (2 (one-of (list 2 8 22)))
 (8 ?))

(narc-expect
 (1 1))

(narc-report)
