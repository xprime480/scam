(narc-label "Type Checker of Symbols")

(narc-expect
 (#t (symbol? 'basic))
 (#t (symbol? '|something unexpected|))
 (#f (symbol? "a string")))

(narc-catch
 (:args (symbol?))
 (:args (symbol? 'one 'two)))

(narc-report)
