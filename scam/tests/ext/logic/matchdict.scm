(import (only (scam unify) match)
        (test narc))

(narc-label "Match Dictionary")

(define the-pattern { :first :name :second :age })
(define the-data    { :second 58 :first "Mike" })

(narc-expect
 ({ :name "Mike" :age 58 } (match the-pattern the-data)))

(narc-report)
