(import (lib test narc))

(narc-label "Import Library Rename")

(narc-catch
 (:eval (sample))
 (:eval (example)))

(import (rename sample (example howdy)))

(narc-expect
 (123     (sample))
 ("howdy" (howdy)))

(narc-catch
 (:eval (example)))

(narc-report)
