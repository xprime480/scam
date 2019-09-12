(import (only (scheme base) < >)
        (scheme write)
        (test narc))

(narc-label "Unless")

(narc-expect
 ('() (unless #t
        (display 1)
        2))
 ('() (unless (> 3 2)
        (display 1)
        1 2))
 (2   (unless (< 3 2)
        (display 1)
        2)))

(narc-catch
 (:syntax (unless))
 (:syntax (unless #t)))

(narc-report)
