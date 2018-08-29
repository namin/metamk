(load "mk2.scm")
(load "meta.scm")
(load "ex-ndfa.scm")
(load "refl.scm")
(load "test-check.scm")

(define ndfa-program-obj
  (run* (q)
    (fresh (a b)
      (== q  (list '<- a b))
      (ndfa-pclause a b))))

(test-check "refl-ndfa-program-obj"
  ndfa-program-obj
  '((<- (accept _.0) ((initial _.1) (accept2 _.0 _.1)))
    (<- (accept2 () _.0) ((final _.0)))
    (<- (accept2 (_.0 . _.1) _.2)
        ((delta _.2 _.0 _.3) (accept2 _.1 _.3))))
)

((reify-prog ndfa-program-obj) 'p)
;; not testing
