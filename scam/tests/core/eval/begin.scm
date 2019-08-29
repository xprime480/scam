(import (lib test narc))

(narc-label "Begin")

(narc-expect
 (99 (begin 1 2 3 (* 5 4) 99))
 (99 (begin 99))
 ('() (begin)))

(narc-report)
