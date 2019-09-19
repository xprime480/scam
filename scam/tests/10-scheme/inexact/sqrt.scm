(import (only (scheme base) * list map truncate)
        (scheme complex)
        (scheme inexact)
        (test narc))

(narc-label "Sqrt")

(narc-expect
 (#i+2i (sqrt -4))
 (#i0   (sqrt 0))
 (#i2   (sqrt 4))
 ('(#i141421 #i141421)			; close enough?
  (map (lambda (x)
         (truncate (* 1e5 x)))
       (let ((v (sqrt +4i)))
         (list (real-part v)
               (imag-part v)))))
 
 (-inf.0i (sqrt -inf.0))
 (+inf.0  (sqrt +inf.0))
 (#t      (nan? (sqrt +nan.0))))

(narc-catch
 (:args (sqrt "cat")))

(narc-report)
