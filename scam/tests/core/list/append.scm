;;; Append lists
;;;

(narc-label "Append Lists")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args        (test-err-cat (append 2 3)))
 ('()          (append))
 ('(x y)       (append '(x) '(y)))
 ('(a b c d)   (append '(a) '(b c d)))
 ('(a (b) (c)) (append '(a (b)) '((c))))
 ('(a b c . d) (append '(a b) '(c . d)))
 ('a           (append '() 'a))
 ('a           (append 'a))
 ('(a b c)     (append '(a b c) '())))

(narc-report)
