;;; Ensure bad values are reported
;;;

(narc-label "Negative Tests for Tokenizing")

(narc-expect
 ("Malformed boolean: {#tt}" #tt))

(narc-report)
