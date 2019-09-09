;;; Environments are captured correctly
;;;
;;; Distinguish between the environments where the class was defined,
;;; where the instance was created, and where the method was invoked.
;;;

(import (only (scheme base) list)
        (only (scam class) make-class)
        (test narc))

(narc-label "Captured Env in Class")

(define (foo)
  (let* ((x 2)
         (Trivial
          (let ((x 3))
            (make-class
             Root
             (q)
             (init (n) (set! q n))
             (get (n) (list q x n)))))
         (obj (Trivial x)))
    (let ((x 5))
      (obj get x))))

(narc-expect
 ('(2 3 5) (foo)))

(narc-report)
