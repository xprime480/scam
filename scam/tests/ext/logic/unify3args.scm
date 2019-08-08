;;; Unify with an initial dictionary
;;;

(narc-label "Unify With Initial Dictionary")

(narc-expect
 ({ :X 5 :Y 8 } (unify :Y 8 { :X 5 })))

(narc-report)
