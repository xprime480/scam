;;; Mutation operations via string-set! or string-copy!
;;;

(narc-label "Mutation operations on strings")

(load "lib/test/test-handler.scm")

(define test-immutable "0123456789")
(define test-mutable (string-copy test-immutable))

(narc-expect
 (:args        (test-err-cat (string-set! :not-a-string 1 #\.)))
 (:args        (test-err-cat (string-set! test-mutable -1 #\.)))
 (:args        (test-err-cat (string-set! test-mutable 1 :not-char)))
 (:args        (test-err-cat (string-set! test-immutable 1 #\.)))

 ("0123.56789" (string-set! test-mutable 4 #\.))

 (:args        (test-err-cat (string-copy! test-immutable 0 "***")))

 ("***3.56789" (string-copy! test-mutable 0 "***"))
 ("***3.567a9" (string-copy! test-mutable 8 "abcdefgh" 0 1))
 ("***3.567a9" (string-copy! test-mutable 0 "abcdefgh" 0 0))

 (:args        (test-err-cat (string-copy! test-mutable 9 "abcdefgh" 3)))
 (:args        (test-err-cat (string-fill! test-immutable #\_)))

 ("***3.567__" (string-fill! test-mutable #\_ 8 10))
 ("***3.56+++" (string-fill! test-mutable #\+ 7))
 ("::::::::::" (string-fill! test-mutable #\:))

 ("0123456789" test-immutable)
 ("::::::::::" test-mutable))

(narc-report)
