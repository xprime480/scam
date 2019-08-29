(import (lib test narc))

(narc-label "Raise")

(narc-catch
 (:args  (error))
 (:args  (error 1 2 3))
 (:user  (error "bagels are not doughnuts"))
 (:other (error :other "bagels are not doughnuts")))

(narc-report)
