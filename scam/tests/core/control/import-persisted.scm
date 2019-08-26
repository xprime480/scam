
(load "lib2.def")

(import (lib2))

(narc-label "Library Import")

(narc-expect
 (4 (x)))

(narc-report)
