(import (only (scheme base) + list)
        (scheme eval)
        (scheme repl)
        (test narc))

(narc-label "Basic Eval")

(define x 1)

(define local-env (interaction-environment))

(narc-expect
 (2      (eval 2    local-env))
 (:foo   (eval :foo local-env))
 ('()    (eval '()  local-env))
 ('(2 4) (let ((x 2))
           (list x
                 (eval `(+ x 3)
                       local-env)))))

(narc-report)
