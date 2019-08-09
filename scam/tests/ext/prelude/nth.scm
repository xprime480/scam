;;; Nth function
;;;

(narc-label "Nth")

(load "lib/prelude.scm")

(narc-expect
 ('a (nth 0 '(a b c)))
 (1  (nth 0 #(1 2 3))))

(narc-catch
 (:args (nth 4 '(a b c)))
 (:args (nth -50 #(1 2 3)))
 (:args (nth 0 { :a 333.333 })))

(narc-report)
