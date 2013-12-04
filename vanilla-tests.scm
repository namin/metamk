(load "vanilla.scm")
(load "ex-peano.scm")
(load "ex-stlc.scm")
(load "test-check.scm")

(test-check "pluso-vanilla"
  (run 3 (q)
    (fresh (a b c)
      (== q `(,a ,b ,c))
      ((vanilla pluso-clause)
       `(pluso ,a ,b ,c))))
  '((z _.0 _.0)
    ((s z) _.0 (s _.0))
    ((s (s z)) _.0 (s (s _.0)))))

(test-check "stlc-vanilla"
  (run 4 (q) (eigen (alpha)
               ((vanilla !-o-clause)
                `(!-o () ,q (,alpha -> ,alpha)))))
  '(((lambda (_.0) _.0) (sym _.0))
    ((lambda (_.0) ((lambda (_.1) _.1) _.0)) (sym _.0 _.1))
    (((lambda (_.0) _.0) (lambda (_.1) _.1)) (sym _.0 _.1))
    ((lambda (_.0) ((lambda (_.1) _.0) _.0)) (=/= ((_.0 _.1))) (sym _.0))))
