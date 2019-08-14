(narc-label "Import Library Errors")

(narc-catch
 (:import (import nothing))
 (:import (import (not-real)))
 (:import (import (only nothing)))
 (:import (import (only sample no-such-function)))
 (:import (import (except sample no-such-function)))
 (:import (import (rename sample (no-such-function whatever)))))

(narc-report)

