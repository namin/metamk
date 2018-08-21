(defrel
  (patho x y)
  ((patho-clause patho))
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


