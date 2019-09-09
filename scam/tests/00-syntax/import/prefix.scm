(import (test narc))

(narc-label "Import Library Prefix")

(narc-catch
 (:eval (sample))
 (:eval (example)))

(import (prefix sample yo-))

(narc-expect
 (123     (yo-sample))
 ("howdy" (yo-example)))

(narc-catch
 (:eval (sample))
 (:eval (example)))

(narc-report)
