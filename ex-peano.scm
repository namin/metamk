(load "meta.scm")

(defrel
  ((pluso-clause pluso))
  (pluso a b c)
  ()
  (conde
    ((== a 'z) (== b c))
    ((fresh (a-1 c-1)
       (== a `(s ,a-1))
       (== c `(s ,c-1))
       (pluso a-1 b c-1)))))
