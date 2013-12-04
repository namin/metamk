(load "mk.scm")
(load "utils.scm")

(define (proofdebug* clause)
  (define (solve* goals proof ok)
    (conde
      ((== goals '())
       (== proof '()))
     ((fresh (first-goal other-goals first-body body-proof other-proof rule-case)
        (== (cons first-goal other-goals) goals)
        (conda
          ((clause first-goal first-body rule-case)
           (== `((,first-goal ,rule-case <-- ,body-proof) . ,other-proof) proof)
           (solve* first-body body-proof ok))
          ((== `((,first-goal error) . ,other-proof) proof)
           (== ok #f)))
        (solve* other-goals other-proof ok)))))
  solve*)

(define (proofdebug clause)
  (let ((solve* (proofdebug* clause)))
    (lambda (goal proof ok)
      (fresh ()
        (solve* (list goal) proof ok)
        (conda
          ((== ok #t))
          ((== ok #f)))))))
