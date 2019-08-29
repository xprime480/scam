(import (lib test narc))

(narc-label "LetRec")

(define-syntax test
  (syntax-rules ()
      ((test sym)
       (sym ((factorial
              (lambda (n)
                (if (> n 1)
                    (* n (factorial (- n 1)))
                    1))))
            (factorial 3)))))

(narc-expect
 (6 (test letrec)))

(narc-catch
 (:eval (test let))
 (:eval (test let*)))

(narc-report)

