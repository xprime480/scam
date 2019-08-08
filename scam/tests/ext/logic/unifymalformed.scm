;;; Unify two patterns with mutual (nonterminating) recursion
;;;

(narc-label "Unify Malformed")

(define result (unify #(:X :Y) #(:Y :X)))

(narc-expect
 ('(#f "Infinite Loop resolving keyword :Y") (result :get :X))
 ('(#f "Infinite Loop resolving keyword :X") (result :get :Y)))

(narc-report)
