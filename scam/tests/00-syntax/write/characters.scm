(import (only (scheme base) get-output-string open-output-string)
        (scheme write)
        (test narc))

(narc-label "Write Character Literals")

(define sink (open-output-string))

(display #\x20 sink)
(display " "   sink)
(display #\    sink)
(display " "   sink)
(display #\x41 sink)
(display " "   sink)
(display #\xFe sink)

(narc-expect
 ("#\space #\space #\A #\xfe" (get-output-string sink)))

(narc-report)
