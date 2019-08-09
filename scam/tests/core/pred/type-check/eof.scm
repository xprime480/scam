;;; Test the eof-object? function
;;;

(narc-label "Type Checker of EOF")

(narc-expect
 (#f    (eof-object? 2))
 (#t    (eof-object? (eof-object))))

(narc-catch
 (:args (eof-object?))
 (:args (eof-object? '(a . b) (list 1 2 3))))

(narc-report)
