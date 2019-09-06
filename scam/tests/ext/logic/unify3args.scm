(import (test narc))

(narc-label "Unify With Initial Dictionary")

(narc-expect
 ({ :X 5 :Y 8 } (unify :Y 8 { :X 5 })))

(narc-report)
