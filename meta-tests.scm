(load "mk2.scm")
(load "meta.scm")
(load "ex-peano.scm")
(load "test-check.scm")

(test-check "pluso-clause"
  (run* (q)
    (fresh (head tail)
      (== q `(,head ,tail))
      (pluso-clause head tail)))
  '(((pluso z _.0 _.0) ())
    ((pluso (s _.0) _.1 (s _.2)) ((pluso _.0 _.1 _.2)))))
