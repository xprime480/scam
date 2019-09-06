(import (test narc))

(narc-label "Substitute")

(narc-expect
 (23           (substitute :X { :X 23 }))
 ('(23 99)     (substitute '(:X 99) { :X 23 }))
 (#(23 99)     (substitute #(:X 99) { :X 23 }))
 (#(23 cat 23) (substitute #(:X :Y :X) { :X 23 :Y cat })))

(narc-catch
 (:dict (substitute :X {})))

(narc-report)
