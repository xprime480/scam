(import (test narc))

(narc-label "Unify Complex Pattern (1)")

(define p1 #(:F #(:A 1  :B) #(:A :B) 2))
(define p2 #(:C #(:D :D :E) :C     :E))

(define final (unify p1 p2))

(narc-expect
 (1      (final :get :A))
 (2      (final :get :B))
 (#(1 2) (final :get :C))
 (1      (final :get :D))
 (2      (final :get :E))
 (#(1 2) (final :get :F)))

(narc-report)
