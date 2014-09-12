;; SICP Chapter 4 section 4 (4.4) Exercises

;;Exercise 4.58 "big shot"

(rule (big-shot ?person ?division)
      (and
       (job ?person (?division . ?r))
       (or (and (supervisor ?person ?supe)
                (job ?supe (?other-div . ?w))
                (not (same ?other-div ?division)))
           (not (supervisor ?person ?supe)))))



;; Ex 4.61
(rule (last-pair (?h . ?t) ?z)
      (last-pair ?t  ?z))
(rule (last-pair (?u) ?u))


;;Ex 4.63
(rule (grandson ?g ?s)
      (and (son ?f ?s)
           (son ?g ?f)))
(rule (son ?m ?s)
      (wife ?m ?w)
      (son ?w ?s))

;; The reverse rule
(rule (reverse () ()))

;;(rule (reverse (?x) (?x)))

(rule (reverse (?h . ?t) ?r)
      (and (append-to-form ?r2 (?h) ?r)
           (reverse ?t ?r2)))
