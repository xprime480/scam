(import (test narc))

(narc-label "CMP <> String")

(narc-expect
 (#t (<> "A"))
 (#f (<> "A" "A"))
 (#t (<> "A" "Z"))

 (#t (<> "A" "Z" "A"))
 (#f (<> "A" "A" "Z")))

(narc-report)

