(import lib/alist
        (lib test narc))

(narc-label "AList")

(define alist (AssocList))

(narc-expect
 (0        (alist length))
 (1        (begin
             (alist put :dog "woof")
             (alist length)))
 ("woof"   (alist get :dog))
 ('()      (alist get :cat))
 (1        (begin
             (alist put :dog "bowwow")
             (alist length)))
 ("bowwow" (alist get :dog))
 (0        (begin
             (alist remove :dog)
             (alist length))))

(narc-report)
