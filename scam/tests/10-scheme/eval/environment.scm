(import (scheme eval)
        (scheme repl)
        (test narc))

(narc-label "Basic Eval")

(define x 1)

(define test-env
  (environment '(only (scheme base) +)))

(narc-expect
 (2    (eval 2    test-env))
 (:foo (eval :foo test-env))
 ('()  (eval '()  test-env))
 (4    (eval '(+ 1 3) test-env)))

(narc-report)
