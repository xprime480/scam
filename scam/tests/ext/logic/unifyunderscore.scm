(import (only (scam unify)
              unify)
        (test narc))

(narc-label "Unify Underscore")

(narc-expect
 ({ :Y 3 } (unify #(:: 3)
                  #('(this is :to :be :ignored) :Y))))

(narc-report)
