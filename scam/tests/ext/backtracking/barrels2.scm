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

(import lib/prelude)

(narc-label "Barrels")

(define (sum lst)
  (apply + lst))

(define (barrels-of-fun)
  (let* ((barrels (list 30 32 36 38 40 62))
         (beer (eval `(amb ,@barrels) (interaction-environment)))
         (wine (exclude (list beer) barrels))
         (barrel-1 (eval `(amb ,@wine)
                         (interaction-environment)))
         (barrel-2 (eval `(amb ,@(exclude (list barrel-1) wine))
                         (interaction-environment)))
         (purchase (some-of (exclude (list barrel-1 barrel-2) wine))))
    (begin
      (require (= (* 2 (+ barrel-1 barrel-2))
                  (sum purchase)))
      beer)))

(narc-expect
 (40 (barrels-of-fun)))

(narc-report)
