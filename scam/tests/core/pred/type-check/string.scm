(import (lib test narc))

(narc-label "Type Checker of Strings")

(narc-expect
 (#t (string? ""))
 (#t (string? "a-string"))
 (#f (string? #\a))
 (#f (string? 2)))

(narc-catch
 (:args (string?))
 (:args (string? "x" "y")))

(narc-report)
