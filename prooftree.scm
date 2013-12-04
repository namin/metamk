(load "mk.scm")
(load "utils.scm")

(define (prooftree* clause)
  (define (solve* goals proof)
    (conde
      ((== goals '())
       (== proof '()))
     ((fresh (first-goal other-goals first-body body-proof other-proof)
        (== (cons first-goal other-goals) goals)
        (== `((,first-goal <-- ,body-proof) . ,other-proof) proof)
        (clause first-goal first-body)
        (solve* first-body body-proof)
        (solve* other-goals other-proof)))))
  solve*)

(define prooftree (one-goal prooftree*))
