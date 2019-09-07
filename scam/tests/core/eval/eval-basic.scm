(import (scheme eval)
        (scheme repl)
        (test narc))

(narc-label "Basic Eval")

(define x 1)

(narc-expect
 (2      (eval 2 (interaction-environment)))
 (:foo   (eval :foo (interaction-environment)))
 ('()    (eval '() (interaction-environment)))
 ('(2 4) (let ((x 2))
           (list x
                 (eval `(+ x 3) (interaction-environment))))))

(narc-report)
