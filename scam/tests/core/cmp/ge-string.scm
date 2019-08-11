(narc-label "CMP >= String")

(narc-expect
 (#t (>= "a"))
 (#t (>= "a" "a"))
 (#t (>= "b" "a"))

 (#t (>= "za" "fa" "ez" ""))
 (#f (>= "zzz" "a" "z" "q")))

(narc-report)

