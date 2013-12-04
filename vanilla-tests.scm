(load "vanilla.scm")
(load "ex-peano.scm")
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
