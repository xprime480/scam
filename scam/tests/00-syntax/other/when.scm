(import (only (scheme base) < >)
        (scheme write)
        (test narc))

(narc-label "When")

(narc-expect
 (2   (when #t
        (display 1)
        2))
 (2   (when (> 3 2)
        (display 1)
        1 2))
 ('() (when (< 3 2)
        (display 1)
        2)))

(narc-catch
 (:syntax (when))
 (:syntax (when #t)))

(narc-report)
