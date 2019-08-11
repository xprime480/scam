;;; Pythagorean Triples from Bill Hails
;;;
;;; Find Some set of pythagorean triples.
;;; The algorithm allows an infinite number of them
;;;

(narc-label "Pythag")

(load "lib/prelude.scm")
(load "lib/numeric.scm")

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

(define port (open-output-string))
(define foo (lambda (x)
              (display x port)
              (display " " port)))

(foo (pythagorean-triples))
(backtrack)
(backtrack)
(backtrack)
(backtrack)

(define expected
  "((x 3) (y 4) (z 5)) ((x 6) (y 8) (z 10)) ((x 5) (y 12) (z 13)) ((x 9) (y 12) (z 15)) ((x 8) (y 15) (z 17)) ")

(narc-expect
 (expected (get-output-string port)))

(narc-report)
