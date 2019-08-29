(import (lib test narc))

(narc-label "Read Empty")

(narc-expect
 (#t (eof-object? (read (open-input-string "")))))

(narc-report)
