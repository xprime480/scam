;;; Unify two empty patterns
;;;

(narc-label "Unify Empty")

(narc-expect
 ({} (unify () ())))

(narc-report)
