;; SICP Chapter 4 section 4 (4.4) Exercises

;;Exercise 4.58 "big shot"

(rule (big-shot ?person ?division)
      (and
       (job ?person (?division . ?r))
       (or (and (supervisor ?person ?supe)
                (job ?supe (?other-div . ?w))
                (not (same ?other-div ?division)))
           (not (supervisor ?person ?supe)))))



