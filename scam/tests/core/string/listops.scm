;;; Test string->list and list->string
;;;

(narc-label "List-String conversions")


(load "lib/test/test-handler.scm")

(narc-expect
 ('()            (string->list ""))
 ('()            (string->list "" 0 0))
 ('(#\a #\b #\c) (string->list "abc"))
 ('(#\b #\c)     (string->list "abc" 1))
 ('(#\b)         (string->list "abc" 1 2))

 (""             (list->string '()))
 ("foo"          (list->string '(#\f #\o #\o)))

 (:args          (test-err-cat (list->string 2)))
 (:args          (test-err-cat (list->string '(#\e 2 #\x)))))

(narc-report)
