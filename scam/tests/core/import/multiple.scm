(import (test narc))

(narc-label "Import Library Multiple")

(narc-catch
 ;;(:eval (max 17 42))
 (:eval (sample))
 (:eval (example)))

(import sample (only (scheme base) max))

(narc-expect
 (123     (sample))
 ("howdy" (example))
 (42      (max 17 42)))

(narc-report)
