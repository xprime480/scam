(import (lib test narc))

(narc-label "Handler Not Used")

(narc-expect
 (:hello (call/cc
          (lambda (k)
            (with-exception-handler
             (lambda (err)
               (k (list
                   (error-object? err)
                   (error-object-message err)
                   (error-object-irritants err))))
             (lambda ()
               :hello))))))

(narc-report)
