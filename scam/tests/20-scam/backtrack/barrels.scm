;;; Barrels of Fun problem from Bill Hails
;;;
;;; Barrels of Fun
;;;
;;; A wine merchant has six barrels of wine and beer containing:
;;;         * 30 gallons
;;;         * 32 gallons
;;;         * 36 gallons
;;;         * 38 gallons
;;;         * 40 gallons
;;;         * 62 gallons
;;;
;;; Five barrels are filled with wine and one with beer. The first customer
;;; purchases two barrels of wine. The second customer purchases twice as much
;;; wine as the first customer. Which barrel contains beer?
;;;

(import (only (scheme base) + * = list)
        (only (scam backtrack) exclude one-of require)
        (only (scam misc) some-of)
        (only (scam extra numeric) sum)
        (test narc))

(narc-label "Barrels")

(define (barrels-of-fun)
  (let* ((barrels (list 30 32 36 38 40 62))
         (beer (one-of barrels))
         (wine (exclude (list beer) barrels))
         (barrel-1 (one-of wine))
         (barrel-2 (one-of (exclude (list barrel-1) wine)))
         (purchase (some-of (exclude (list barrel-1 barrel-2) wine))))
    (begin
      ;;(trace (list barrel-1 barrel-2 (sum purchase)))
      (require (= (* 2 (+ barrel-1 barrel-2))
                  (sum purchase)))
      ;;(trace (list "solution: " beer))
      beer)))

(narc-expect
 (40 (barrels-of-fun)))

(narc-report)
