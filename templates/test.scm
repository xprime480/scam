;;; Comments for the human users
;;;

(import (test narc))

(narc-label "One Line for reporting")

(narc-expect
 (value expression) ...
 )

(narc-catch
 (:error-category expression) ...
 )

(narc-report)

