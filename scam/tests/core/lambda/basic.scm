;;; Create Basic Lambda Values
;;;

(narc-label "Basic Lambda Expressions")

(narc-expect
 ('(2 2) (let* ((proc (lambda () 2))
                (args ()))
           (list
            (proc)
            (apply proc args))))
 ('(#\z #\z) (let* ((proc (lambda () 2 #\z))
                    (args ()))
               (list
                (proc)
                (apply proc args))))

 ('(6 6) (let* ((proc (lambda (x) (+ x x)))
                (args '(3)))
           (list
            (proc 3)
            (apply proc args)))))


(narc-report)
