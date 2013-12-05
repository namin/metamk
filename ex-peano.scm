(load "meta.scm")

(defrel
  (pluso a b c)
  ((pluso-clause pluso))
  ()
  (conde
    ((== a 'z) (== b c))
    ((fresh (a-1 c-1)
       (== a `(s ,a-1))
       (== c `(s ,c-1))
       (pluso a-1 b c-1)))))

(define (pluso-clause-case head tail rule-case)
  (fresh ()
    (== rule-case '())
    (pluso-clause head tail)))
