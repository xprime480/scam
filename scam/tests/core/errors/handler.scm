(import (lib test narc))

(narc-label "Handler")

(narc-expect
 ('(#t "Message %{0}" (:baba))
  (call/cc
   (lambda (k)
     (with-exception-handler
      (lambda (err)
        (k (list
            (error-object? err)
            (error-object-message err)
            (error-object-irritants err))))
      (lambda ()
        (error "Message %{0}" :baba)))))))

(narc-report)

