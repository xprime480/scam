;;; Pythagorean Triples from Bill Hails
;;;
;;; Find Some set of pythagorean triples.
;;;

(narc-label "Pythag")

(load "lib/prelude.scm")
(load "lib/numeric.scm")

(define pythagorean-triples
  (lambda ()
    (let ((x (amb 1 2 3 4 5 6 7 8))
          (y (amb 1 2 3 4 5 6 7 8))
          (z (amb 1 2 3 4 5 6 7 8 9 10 11 12)))
      (begin
        (require (= (+ (square x)
                       (square y))
                    (square z)))
        `((x ,x) (y ,y) (z ,z))))))

(narc-skip
 ('((x 3) (y 4) (z 5))   (pythagorean-triples))
 ('((x 4) (y 3) (z 5))   ?)
 ('((x 6) (y 8) (z 10))  ?)
 ('((x 8) (y 6) (z 10))  ?)
 ("No more choices"      ?))

(narc-report)
