(import (lib test narc))

(narc-label "Let Errors")

(define-syntax master
  (syntax-rules ()
    ((master f)
     (narc-catch
      (:args (f let))
      (:args (f let*))
      (:args (f letrec))))))

(define-syntax bad1
  (syntax-rules ()
      ((test sym)
       (sym x 2))))

(define-syntax bad2
  (syntax-rules ()
      ((test sym)
       (sym (3 2) "foo"))))

(define-syntax bad3
  (syntax-rules ()
      ((test sym)
       (sym ((3 2)) "foo"))))

(define-syntax bad4
  (syntax-rules ()
      ((test sym)
       (sym ((x)) "foo"))))

(master bad1)
(master bad2)
(master bad3)
(master bad4)

(narc-report)

