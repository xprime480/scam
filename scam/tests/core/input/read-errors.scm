(import (test narc))

(narc-label "Read Errors")

(narc-catch
 (:args (read))
 (:args (read (open-input-string "") :nope))
 (:args (read 2))
 
 (:read (read (open-input-string "(2 3 4")))
 (:read (read (open-input-string ")"))))

(narc-report)
