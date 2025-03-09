(define (appendo xs ys zs)
  (conde
    ((== xs '())
     (== ys zs))
    ((fresh (x xr zr)
       (== (cons x xr) xs)
       (== (cons x zr) zs)
       (appendo xr ys zr)))))
