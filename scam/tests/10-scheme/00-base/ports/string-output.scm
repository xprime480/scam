(import (only (scheme base) get-output-string open-output-string)
        (scheme write)
        (test narc))

(narc-label "String Output Port")

(define sink (open-output-string))

(display 1 sink)
(display " " sink)
(display 2 sink)
(display " " sink)
(display 3 sink)

(narc-expect
 ("1 2 3" (get-output-string sink)))

(narc-report)
