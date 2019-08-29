;;; Pythagorean Triples from Bill Hails
;;;
;;; Find Some set of pythagorean triples.
;;;

(import lib/prelude
        lib/numeric
        lib/test/value_helper
        (lib test narc))

(narc-label "Pythag")

(define (pythagorean-triples)
  (let ((x (amb 1 2 3 4 5 6 7 8))
        (y (amb 1 2 3 4 5 6 7 8))
        (z (amb 1 2 3 4 5 6 7 8 9 10 11 12)))
    (begin
      (require (= (+ (square x)
                     (square y))
                  (square z)))
      `((x ,x) (y ,y) (z ,z)))))

(define helper (ValueHelper))

(helper update (pythagorean-triples))
(backtrack)
(backtrack)
(backtrack)

(narc-catch
 (:values (backtrack)))

(define expected (let ((p (ValueHelper)))
                   (p update '((x 3) (y 4) (z 5)))
                   (p update '((x 4) (y 3) (z 5)))
                   (p update '((x 6) (y 8) (z 10)))
                   (p update '((x 8) (y 6) (z 10)))
                   (p get)))

(narc-expect
 (expected (helper get)))

(narc-report)
