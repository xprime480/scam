;;; Make errors
;;;

(narc-label "Make Error")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (make-error)))
 (:args (test-err-cat (make-error 1 2 3)))
 ("bagels are not doughnuts"
  (make-error "bagels are not doughnuts"))
 ("wanted a list, got 2 3 4"
  (make-error "wanted a list, got %{0} %{2} %{1}" 2 4 3)))

(narc-report)





