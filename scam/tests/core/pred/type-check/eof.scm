;;; Test the eof-object? function
;;;

(narc-label "Type Checker of EOF")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (eof-object?)))
 (:args (test-err-cat (eof-object? '(a . b) (list 1 2 3))))
 (#f    (eof-object? 2))
 (#t    (eof-object? (eof-object))))

(narc-report)
