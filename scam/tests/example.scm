;;; Test functions to copy substrings
;;;

(load "lib/prelude.scm")

(narc-label "Substring functions")

(define empty "")
(define hello "Hello, World")

(narc-expect
 (""
  (string-copy empty 0 0))
 (""
  (string-copy hello 0 0))
 (""
  (string-copy hello 1 1))
 ("ll"
  (string-copy hello 2 4))
 ("ll"
  (substring hello 2 4))
 ("World"
  (string-copy hello 7))
 ("Hello, World"
  (string-copy hello)))

(narc-catch
 ("string-copy: Expected 1 values, got 3"
  (string-copy hello -1 555))
 ("string-copy: Expected 2 values, got 3"
  (string-copy hello 0 555))
 ("string-copy: Expected 1 values, got 3"
  (string-copy hello 5 0)))

(narc-report)