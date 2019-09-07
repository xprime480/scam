(import (only (scam backtrack)
              amb
              backtrack)
        (test value-helper)
        (test narc))

(narc-label "AMB Nested Let*")

(define helper (ValueHelper))

(helper update (let* ((fst (amb 'a 'b))
                      (snd (amb 1 2)))
                 (list fst snd)))
(backtrack)
(backtrack)
(backtrack)

(narc-catch
 (:values (backtrack)))

(define expected (let ((p (ValueHelper)))
                   (p update '(a 1))
                   (p update '(a 2))
                   (p update '(b 1))
                   (p update '(b 2))
                   (p get)))

(narc-expect
 (expected (helper get)))

(narc-report)
