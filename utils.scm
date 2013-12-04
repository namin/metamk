(define one-goal
  (lambda (meta-ev)
    (lambda (clause)
      (lambda (goal . args)
        (apply (meta-ev clause) (cons (list goal) args))))))
