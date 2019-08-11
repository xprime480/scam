(narc-label "CMP < String")

(narc-expect
 (#t (< "a"))
 (#f (< "a" "a"))
 (#t (< "a" "b")))

(narc-report)

