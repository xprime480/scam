(import (lib test narc))

(narc-label "Dictionary Operations")

(define aDict {})

(narc-expect
 ({ :key 22 }
  (begin
    (aDict :put :key 22)
    aDict))

 ({ :key 22 fun "hello" }
  (begin
    (aDict :put fun "hello")
    aDict))

 ({ :key -1.5 fun "hello" }
  (begin
    (aDict :put :key -1.5)
    aDict))

 (-1.5 (aDict :get :key))

 (1
  (begin
    (aDict :remove :key)
    (aDict :length)))

 ({ fun "hello" }
  aDict))

(narc-report)
