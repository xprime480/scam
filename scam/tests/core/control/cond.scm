;;; test cond syntax
;;;

(load "lib/prelude.scm")
(define (inc x)
  (+ x 1))

(narc-label "Cond")

(narc-expect
 ("One" (cond
         (#t "One")))
 (() (cond
      (#f "One")))
 ("Last" (cond
          (#f "One")
          ((< 2 1) "Two")
          ((= 2 3) "Three")
          (#t "Last")))
 ("Else Evaluated" (cond
                    (#f "One")
                    (else "Else Evaluated")))
 (3 (cond
     (#t 1 2 3)))
 (3 (cond
     (3)))
 (2 (cond
     (1 => inc))))

(narc-catch
 (:syntax (cond ())))

(narc-report)
