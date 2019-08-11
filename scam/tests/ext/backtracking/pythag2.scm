;;; Pythagorean Triples from Bill Hails
;;;
;;; Find Some set of pythagorean triples.
;;; The algorithm allows an infinite number of them
;;;

(narc-label "Pythag")

(load "lib/prelude.scm")
(load "lib/numeric.scm")
(load "lib/test/value_helper.scm")

(define pythagorean-triples
  (lambda ()
    (let* ((z (integers-from 1))
           (x (integers-between 1 z))
           (y (integers-between x z)))
      (begin
        (require (= (+ (square x)
                       (square y))
                    (square z)))
        `((x ,x) (y ,y) (z ,z))))))

(define helper (ValueHelper))

(helper update (pythagorean-triples))
(backtrack)
(backtrack)
(backtrack)
(backtrack)

(define expected (let ((p (ValueHelper)))
                   (p update '((x 3) (y 4) (z 5)))
                   (p update '((x 6) (y 8) (z 10)))
                   (p update '((x 5) (y 12) (z 13)))
                   (p update '((x 9) (y 12) (z 15)))
                   (p update '((x 8) (y 15) (z 17)))
                   (p get)))

(narc-expect
 (expected (helper get)))

(narc-report)
