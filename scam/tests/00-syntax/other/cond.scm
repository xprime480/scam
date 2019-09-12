(import (only (scheme base) + * < =)
        (test narc))

(define (inc x)
  (+ x 1))

(narc-label "Cond")

(narc-expect
 ("One" (cond
         (#t "One")))

 ('() (cond
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
     (1 => inc)))
 (3 (cond
     (1 => (lambda (x) (* x 3))))))

(narc-catch
 (:args   (cond)))

(narc-report)
