(load "meta.scm")
(load "test-check.scm")

(defrel (pluso)
   pluso-clause
   (pluso a b c)
   (conde
    ((== a 'z) (== b c))
    ((fresh (a-1 c-1)
            (== a `(s ,a-1))
            (== c `(s ,c-1))
            (pluso a-1 b c-1)))))

(test-check "pluso-clause"
  (run* (q)
        (fresh (head tail)
               (== q `(,head ,tail))
               (pluso-clause head tail)))
  '(((pluso z _.0 _.0) ())
    ((pluso (s _.0) _.1 (s _.2)) ((pluso _.0 _.1 _.2)))))
