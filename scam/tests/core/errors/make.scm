(import (scam error)
        (test narc))

(narc-label "Make Error")

(narc-expect
 (#t
  (error-object? (make-error "bagels are not doughnuts")))
 ("bagels are not doughnuts"
  (error->string (make-error "bagels are not doughnuts")))
 ("wanted a list, got 2 3 4"
  (error->string (make-error "wanted a list, got %{0} %{2} %{1}" 2 4 3))))

(narc-catch
 (:args (make-error))
 (:args (make-error 1 2 3)))

(narc-report)





