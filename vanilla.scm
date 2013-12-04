(load "mk.scm")
(load "utils.scm")

(define (vanilla* clause)
  (define (solve* goals)
    (conde
      ((== goals '()))
      ((fresh (first-goal other-goals first-body)
         (== (cons first-goal other-goals) goals)
         (clause first-goal first-body)
         (solve* first-body)
         (solve* other-goals)))))
  solve*)

(define vanilla (one-goal vanilla*))
