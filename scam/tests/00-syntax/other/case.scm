(import (only (scheme base) + *)
        (test narc))

(define (inc x)
  (+ x 1))

(narc-label "Case")

(narc-expect
 ('composite (case (* 2 3)
               ((2 3 5 7) 'prime)
               ((1 4 6 8 9) 'composite)))

 (2 (case 1
      ((2 4 6 8) 0)
      ((1 3 5) => inc)))
 (2 (case 1
      ((2 4 6 8) 0)
      (else => (lambda (x) (* 2 x)))))

 ('foo (case 1
         ((2 4 6 8) 0)
         (else 'a 'b 'c 'foo)))

 ('foo (case 1
         ((2 4 6 8) 0)
         ((7 1) 1 2 3 'foo)))

 ('() (case 1
         ((2 4 6 8) 0))))

(narc-catch
 (:args (case ())))

(narc-report)
