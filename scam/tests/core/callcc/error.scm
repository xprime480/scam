;;; Call/CC invoked in various incorrect ways
;;;

(narc-label "Bad Continuation Invocation")

(narc-catch
 (:eval (call/cc 2))
 (:args (+ 4 (call/cc (lambda (k) (k)))))
 (:args (+ 4 (call/cc (lambda (k) (k 1 2))))))

(narc-report)
