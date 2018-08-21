;; from The Art of Prolog

;; Program 17.1
;; An interpreter for a nondeterministic finite automaton (NDFA)

(defrel
  (accept xs)
  ((accept-clause accept accept2 initial delta final))
  ()
  (fresh (q)
    (initial q)
    (accept2 xs q)))

(defrel
  (accept2 xs q)
  ((accept2-clause accept2 initial delta final))
  ()
  (conde
    ((fresh (x xr q1)
       (== (cons x xr) xs)
       (delta q x q1)
       (accept2 xr q1)))
    ((== xs '())
     (final q))))

;; Program 17.2
;; An NDFA that accepts that language (ab)*

(defrel
  (initial q)
  ((initial-clause initial))
  ()
  (== q 'q0))

(defrel
  (final q)
  ((final-clause final))
  ()
  (== q 'q0))

(defrel
  (delta qa c qb)
  ((delta-clause delta))
  ()
  (conde
    ((== qa 'q0)
     (== c 'a)
     (== qb 'q1))
    ((== qa 'q1)
     (== c 'b)
     (== qb 'q0))))

(define (ndfa-clause a b)
  (fresh (x y z)
    (conde
      ((== `(accept ,x) a)
       (accept-clause a b))
      ((== `(accept2 ,x ,y) a)
       (accept2-clause a b))
      ((== `(initial ,x) a)
       (initial-clause a b))
      ((== `(final ,x) a)
       (final-clause a b))
      ((== `(delta ,x ,y ,z) a)
       (delta-clause a b)))))

(define (should-fold g r)
  (fresh (x y)
    (conde
      ((== g `(accept2 ,x ,y))
       (== r `(ab2 ,x ,y)))
      ((== g `(accept ,x))
       (== r `(ab ,x))))))
(define (should-unfold g)
  (fresh (x y z)
    (conde
      ((== g `(initial ,x)))
      ((== g `(final ,x)))
      ((== g `(delta ,x ,y ,z))))))

(define (ndfa-pclause a b)
  (fresh (x y z)
    (conde
      ((== `(accept ,x) a)
       (accept-clause a b))
      ((== `(accept2 ,x ,y) a)
       (accept2-clause a b)))))
