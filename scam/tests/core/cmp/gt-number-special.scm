(import (only (scheme base) >)
        (test narc))

(narc-label "CMP > Special Numbers")

(narc-expect
 (#t (> -inf.0))
 (#t (> +inf.0))
 (#f (> +nan.0))

 (#t (> +inf.0 0 -inf.0))
 (#f (> -inf.0 -inf.0))
 (#f (> +nan.0 +nan.0)))

(narc-report)

