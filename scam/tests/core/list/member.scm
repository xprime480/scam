(import lib/prelude
	(lib test narc))

(narc-label "Member")

(narc-expect
 (#f (member? 1 (list)))
 (#t (member? 1 (list 5 4 3 2 1)))
 (#f (member? 99 (list 5 4 3 2 1))))

(narc-report)
