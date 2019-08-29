(import (lib test narc))

(narc-label "EOF")

(narc-expect
 (#t (eof-object? (eof-object))))

(narc-catch
 (:args (eof-object 2)))

(narc-report)
