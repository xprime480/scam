(import (lib test narc))

(narc-label "Import Library Complex")

(narc-catch
 (:eval (sample))
 (:eval (example)))

(import (except
	 (rename sample (example howdy))
	 sample))

(narc-expect
 ("howdy" (howdy)))

(narc-catch
 (:eval (sample))
 (:eval (example)))

(narc-report)
