(load "prooftree.scm")
(load "ex-peano.scm")
(load "ex-stlc.scm")
(load "test-check.scm")

(test-check "pluso-prooftree"
  (run 3 (q)
    (fresh (a b c proof)
      (== q `((,a ,b ,c) ,proof))
      ((prooftree pluso-clause)
       `(pluso ,a ,b ,c) proof)))
  '(((z _.0 _.0) (((pluso z _.0 _.0) <-- ())))
    (((s z) _.0 (s _.0))
     (((pluso (s z) _.0 (s _.0))
       <--
       (((pluso z _.0 _.0) <-- ())))))
    (((s (s z)) _.0 (s (s _.0)))
     (((pluso (s (s z)) _.0 (s (s _.0)))
       <--
       (((pluso (s z) _.0 (s _.0))
         <--
         (((pluso z _.0 _.0) <-- ())))))))))

(test-check "stlc-prooftree-no-eigen"
  (run 2 (q)
    (fresh (term proof)
      (== q `(,term ,proof))
      ((prooftree !-o-clause)
       `(!-o () ,term (A -> A))
       proof)))
  '((((lambda (_.0) _.0)
      (((!-o () (lambda (_.0) _.0) (A -> A))
        <--
        (((!-o ((_.0 : A)) _.0 A) <-- ())))))
     (sym _.0))
    (((lambda (_.0) ((lambda (_.1) _.1) _.0))
      (((!-o () (lambda (_.0) ((lambda (_.1) _.1) _.0)) (A -> A))
        <--
        (((!-o ((_.0 : A)) ((lambda (_.1) _.1) _.0) A)
          <--
          (((!-o ((_.0 : A)) (lambda (_.1) _.1) (A -> A))
            <--
            (((!-o ((_.1 : A) (_.0 : A)) _.1 A) <-- ())))
           ((!-o ((_.0 : A)) _.0 A) <-- ())))))))
     (sym _.0 _.1))))
