;;; Lambda captures it definition environment.
;;; In this case 'y' retains its value.
;;;

(narc-label "Lambda With Capture")

(define f ())
(let ((y 5))
  (set! f (lambda (x)
            (* x y))))

(narc-expect
 (20 (f (+ 1 3))))

(narc-report)
