(import (lib test narc))

(narc-label "Import Library Only")

(narc-catch
 (:eval (sample))
 (:eval (example)))

(import (only sample sample))

(narc-expect
 (123  (sample)))

(narc-catch
 (:eval (example)))

(narc-report)
