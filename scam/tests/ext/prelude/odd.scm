(import (only lib/prelude odd?)
        (lib test narc))

(narc-label "Odd?")

(narc-expect
 (#t (odd? -3))
 (#t (odd? 3))
 (#t (odd? 82383))
 (#f (odd? -2))
 (#f (odd? 0))
 (#f (odd? 2))
 (#f (odd? 12398234))
 (#t (odd? 3.0))
 (#f (odd? #t))
 (#f (odd? "Silly, strings don't have parity")))

(narc-report)
