(import (lib test narc))

(narc-label "Import Library")

(narc-catch
 (:eval (sample))
 (:eval (example)))

(import sample)

(narc-expect
 (123     (sample))
 ("howdy" (example)))

(narc-report)
