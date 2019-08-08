;;; Map a pattern over several data structures
;;;

(narc-label "Match Mapping")

(load "lib/prelude.scm")

(define the-pattern #(:name 21))

(define the-data
  '(#("Mike" 58)
    #("Abby" 53)
    #("Morgan" 24)
    #("Connor" 21)))

(define doit
  (lambda ()
    (let ((age21 (lambda (dat)
                   (match the-pattern dat))))
      (car (filter dict?
                   (map age21 the-data))))))

(narc-expect
 ({ :name "Connor" } (doit)))

(narc-report)
