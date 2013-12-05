(load "meta.scm")
(load "matche.scm")

(defrel
  ((patho-clause patho))
  (patho x y)
  ()
  (conde
    [(edgeo x y)]
    [(fresh (z)
       (edgeo x z)
       (patho z y))]))

(defmatche (edgeo x y)
   [(a b)]
   [(b c)]
   [(c a)])


