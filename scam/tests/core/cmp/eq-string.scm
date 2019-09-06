(import (test narc))

(narc-label "CMP = String")

(narc-expect
 (#t (= "A"))
 (#t (= "A" "A"))
 (#f (= "A" "Z"))
 
 (#t (= "A" "A" "A" "A" "A" "A"))
 (#f (= "A" "A" "A" "A" "A" "Z")))

(narc-report)

