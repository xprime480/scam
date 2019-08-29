(import x2
	(lib test narc))

(narc-label "Import Library Recursively")

(narc-expect
 (123     (sample))
 (120     (x2 -3)))

(narc-report)
