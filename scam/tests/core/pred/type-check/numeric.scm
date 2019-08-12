(narc-label "Type Checker of Numbers")

(narc-expect
 (#t (numeric? 17))
 (#t (numeric? 11/3))
 (#t (numeric? 42.5))
 (#t (numeric? +i))
 (#f (numeric? "2")))

(narc-catch
 (:args (numeric?))
 (:args (numeric? 1 2)))

(narc-expect
 (#t (complex? 17))
 (#t (complex? 11/3))
 (#t (complex? 42.5))
 (#t (complex? +i))
 (#f (complex? "2")))

(narc-catch
 (:args (complex?))
 (:args (complex? 1 2)))

(narc-expect
 (#t (real? 17))
 (#t (real? 11/3))
 (#t (real? 42.5))
 (#f (real? +i))
 (#f (real? "2")))

(narc-catch
 (:args (real?))
 (:args (real? 1 2)))

(narc-expect
 (#t (rational? 17))
 (#t (rational? 11/3))
 (#t (rational? 42.5))
 (#f (rational? +i))
 (#f (rational? "2")))

(narc-catch
 (:args (rational?))
 (:args (rational? 1 2)))

(narc-expect
 (#t (integer? 17))
 (#f (integer? 11/3))
 (#f (integer? 42.5))
 (#f (integer? +i))
 (#f (integer? "2")))

(narc-catch
 (:args (integer?))
 (:args (integer? 1 2)))

(narc-report)
