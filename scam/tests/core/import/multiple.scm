(import (lib test narc))

(narc-label "Import Library Multiple")

(narc-catch
 (:eval (sample))
 (:eval (example))
 (:eval (max 17 42)))

(import sample (only lib/prelude max))

(narc-expect
 (123     (sample))
 ("howdy" (example))
 (42      (max 17 42)))

(narc-report)
