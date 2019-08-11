;;; Value accumulator for tests with backtracking
;;;

(define ValueHelper
  (make-class
   Root
   (port)
   (init () (set! port (open-output-string)))
   (update (value) (begin
		     (display value port)
		     (display " " port)
		     #t))
   (get () (get-output-string port))))
