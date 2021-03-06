(import (only (scheme base) call/cc)
        (test narc))

(narc-label "Call/CC")

(narc-expect
 (6 (+ 4 (call/cc (lambda (k) 2))))
 (5 (+ 4 (call/cc
          (lambda (cont)
            (+ (cont 1) 2)))))
 (12 (+ 4 (call/cc
           (lambda (cont)
             (+ (cont (+ 3 5)) 2))))))

(narc-report)
