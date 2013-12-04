(define one-goal
  (lambda (meta-ev)
    (lambda (clause)
      (lambda (goal)
        ((meta-ev clause) (list goal))))))
