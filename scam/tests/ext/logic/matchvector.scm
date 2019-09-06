(import (test narc))

(narc-label "Match Vector")

(narc-expect
 ({ :name "Mike" :age 58 } (match #(:name :age) #("Mike" 58))))

(narc-report)
