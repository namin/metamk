(load "mk2.scm")
(load "meta.scm")
(load "ex-ndfa.scm")
(load "preduce.scm")
(load "refl.scm")
(load "test-check.scm")

(define (ndfa-program p)
  ((refl-prog ndfa-pclause) p))

(test-check "sanity-check-nda-program"
  (run 1 (p)
    (ndfa-program p))
  '(((<- (accept _.0) ((initial _.1) (accept2 _.0 _.1)))
     (<- (accept2 () _.2) ((final _.2)))
     (<- (accept2 (_.3 . _.4) _.5)
         ((delta _.5 _.3 _.6) (accept2 _.4 _.6))))))

(test-check "preduce-ndfa"
  (run 1 (q)
    (fresh (p)
      (ndfa-program p)
      ((preduce* ndfa-clause)
       p
       q)))
  '(((<- ((ab _.0)) ((ab2 _.0 q0)))
     (<- ((ab2 () q0)) ())
     (<- ((ab2 (a . _.1) q0)) ((ab2 _.1 q1))))))
