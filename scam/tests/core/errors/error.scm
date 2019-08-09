;;; Raise errors
;;;

(narc-label "Raise")

(load "lib/test/test-handler.scm")

(narc-catch
 (:args (error))
 (:args (error 1 2 3))
 (:user (error "bagels are not doughnuts"))
 (:other (error :other "bagels are not doughnuts")))

(narc-report)
