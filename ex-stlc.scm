(load "meta.scm")
(load "matche.scm")

(define (m== x y)
  (== x y))

(defrel
  ((!-o-clause-i !-o) (!-o-full-clause-i !-o lookupo o==))
  (!-o gamma expr type)
  (rule-case)
  (matche (expr type)
    ((,x ,T) (symbolo x) (lookupo `(,x : ,T) gamma)
     (m== rule-case 'var))
    (((lambda (,x) ,e) (,T1 -> ,T2))
     (!-o `((,x : ,T1) . ,gamma) e T2)
     (m== rule-case 'abs))
    (((,e1 ,e2) ,T)
     (fresh (T1)
       (!-o gamma e1 `(,T1 -> ,T))
       (!-o gamma e2 T1))
     (m== rule-case 'app))))

(define (!-o-clause head tail)
  (fresh (rule-case)
    (!-o-clause-i head tail rule-case)))

(define (!-o-clause-case head tail rule-case)
  (!-o-clause-i head tail rule-case))

(define (lookupo binding rho)
  (matche (binding rho)
    (((,x : ,v) ((,x : ,v) . ,_)))
    (((,x : ,v) ((,y : ,_) . ,rho1)) (=/= x y) (lookupo binding rho1))))

(define (!-o-debug-clause-case head tail rule-case)
  (conde
    ((!-o-full-clause-i head tail rule-case))
    ((== tail '())
     (== rule-case '())
     (conde
       ((fresh (binding rho)
          (== `(lookupo ,binding ,rho) head)
          (lookupo binding rho)))
       ((fresh (x y)
          (== `(== ,x ,y) head)
          (== x y)))))))
