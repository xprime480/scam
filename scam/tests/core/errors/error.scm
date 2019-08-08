;;; Raise errors
;;;

(narc-label "Raise")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (error)))
 (:args (test-err-cat (error 1 2 3)))
 ("bagels are not doughnuts" (error "bagels are not doughnuts"))
 ("wanted a list, got 2 3 4" (error "wanted a list, got %{0} %{2} %{1}" 2 4 3)))

(narc-report)
