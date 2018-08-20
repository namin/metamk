(load "preduce.scm")
(load "ex-ndfa.scm")
(load "test-check.scm")

(define (appendo xs ys zs)
  (conde
    ((== xs '())
     (== ys zs))
    ((fresh (x xr zr)
       (== (cons x xr) xs)
       (== (cons x zr) zs)
       (appendo xr ys zr)))))

(define ndfa-program-obj
  (run* (q)
    (fresh (a b)
      (== q  (list '<- a b))
      (ndfa-pclause a b))))
;; we need the variables...
(define (ndfa-program p)
  (fresh (_.10 _.11 _.20 _.30 _.31 _.32 _.33)
    (== p
        (list
         `(<- (accept ,_.10) ((initial ,_.11) (accept2 ,_.10 ,_.11)))
         `(<- (accept2 () ,_.20) ((final ,_.20)))
         `(<- (accept2 (,_.30 . ,_.31) ,_.32)
              ((delta ,_.32 ,_.30 ,_.33) (accept2 ,_.31 ,_.33)))))))

(test-check "sanity-check-nda-program"
  (run 1 (p)
    (ndfa-program p))
  '(((<- (accept _.0) ((initial _.1) (accept2 _.0 _.1)))
     (<- (accept2 () _.2) ((final _.2)))
     (<- (accept2 (_.3 . _.4) _.5)
         ((delta _.5 _.3 _.6) (accept2 _.4 _.6))))))

(test-check "preduce-ndfa"
  (run 1 (q)
    (fresh (p)
      (ndfa-program p)
      ((preduce* ndfa-clause)
       p
       q)))
  ;; TODO -- we lost the structure of the program.
  '(((ab2 _.0 q0)
     (ab _.0)
     (ab2 () q0)
     (ab2 _.1 q1)
     (ab2 (a . _.1) q0))))
