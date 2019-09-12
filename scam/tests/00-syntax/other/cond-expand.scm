(import (test narc))

(narc-label "Cond-Expand")

(narc-expect
 ('yes (cond-expand
        ((library (scheme base)) 'yes)
        (else 'no)))
 ('yes (cond-expand
        ((and (library (scheme base))
              (library (scam base))) 'yes)
        (else 'no)))
 ('yes (cond-expand
        ((or (library (scheme base))
             (library (rotten eggs base))) 'yes)
        (else 'no)))
 ('yes (cond-expand
        ((not (library (rotten eggs base))) 'yes)
        (else 'no)))

 ('no (cond-expand
       ((library (rotten eggs base)) 'yes)
       (else 'no)))
 ('no (cond-expand
       ((and (library (scheme base))
             (library (rotten eggs base))) 'yes)
       (else 'no)))
 ('no (cond-expand
       ((or (library (smelly socks base))
            (library (rotten eggs base))) 'yes)
       (else 'no)))
 ('no (cond-expand
       ((not (library (scheme base))) 'yes)
       (else 'no))))

(narc-catch
 (:args (cond-expand
         ('just-silly 'yes)
         (else 'no))))

(narc-report)
