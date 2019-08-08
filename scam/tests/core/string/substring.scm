;;; Test substring
;;;

(narc-label "Substring functions")

(load "lib/test/test-handler.scm")

(load "lib/prelude.scm")

(define empty "")
(define hello "Hello, World")

(narc-expect
 (""              (string-copy empty 0 0))
 (""              (string-copy hello 0 0))
 (""              (string-copy hello 1 1))
 ("ll"            (string-copy hello 2 4))
 ("World"         (string-copy hello 7))
 ("Hello, World"  (string-copy hello))

 (:args           (test-err-cat (string-copy hello -1 555)))
 (:args           (test-err-cat (string-copy hello 0 555)))
 (:args           (test-err-cat (string-copy hello 5 0)))

 ("ll"            (substring hello 2 4)))

(narc-report)
