(import (lib test narc))

(narc-label "Class Without Init")

(define Trivial (make-class
                 Root
                 (n)
                 (get () n)))

(define obj (Trivial))

(narc-expect
 ('() (obj get)))

(narc-report)
