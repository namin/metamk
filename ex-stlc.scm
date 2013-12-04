(load "meta.scm")
(load "matche.scm")

(defrel
  ((!-o-clause-i !-o) (!-o-full-clause-i !-o lookupo ==))
  (!-o gamma expr type)
  (rule-case)
  (matche (expr type)
    ((,x ,T) (symbolo x) (lookupo `(,x : ,T) gamma)
     (== rule-case 'var))
    (((lambda (,x) ,e) (,T1 -> ,T2))
     (!-o `((,x : ,T1) . ,gamma) e T2)
     (== rule-case 'abs))
    (((,e1 ,e2) ,T)
     (fresh (T1)
       (!-o gamma e1 `(,T1 -> ,T))
       (!-o gamma e2 T1))
     (== rule-case 'app))))

(define (!-o-clause head tail)
  (fresh (rule-case)
    (!-o-clause-i head tail rule-case)))

(define (!-o-clause-case head tail rule-case)
  (!-o-clause-i head tail rule-case))

(define (lookupo binding rho)
  (matche (binding rho)
    (((,x : ,v) ((,x : ,v) . ,_)))
    (((,x : ,v) ((,y : ,_) . ,rho1)) (=/= x y) (lookupo binding rho1))))

;; TODO: test it!
(define (!-o-debug-clause head tail)
  (conde
    ((!-o-full-clause head tail))
    ((== tail '())
     (conde
       ((fresh (binding rho)
          (== `(lookupo ,binding ,rho) head)
          (lookupo binding rho)))
       ((fresh (x y)
          (== `(== ,x ,y) head)
          (== x y)))))))
