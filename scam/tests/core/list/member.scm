;;; member function
;;; This is not the member function defined in r7rs
;;;

(narc-label "Member")

(load "lib/prelude.scm")

(narc-expect
 (#f (member? 1 (list)))
 (#t (member? 1 (list 5 4 3 2 1)))
 (#f (member? 99 (list 5 4 3 2 1))))

(narc-report)
