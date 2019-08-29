(import lib/prelude
        (lib test narc))

(narc-label "Require")

(narc-skip
 (#t                (require #t))
 ("No more choices" (require #f)))

(narc-expect
 (1 1))

(narc-report)
