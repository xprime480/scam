;;; End-of-File objects
;;;

(narc-label "EOF")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (eof-object 2)))
;; (eof   (eof-object))
 (#t    (eof-object? (eof-object))))

(narc-report)
