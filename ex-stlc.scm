(load "meta.scm")
(load "matche.scm")


(define (lookupo binding rho)
  (matche (binding rho)
    (((,x : ,v) ((,x : ,v) . ,_)))
    (((,x : ,v) ((,y : ,_) . ,rho1)) (=/= x y) (lookupo binding rho1))))

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

(define (to-clause clause-i)
  (lambda (head tail)
    (fresh (rule-case)
      (clause-i head tail rule-case))))
(define !-o-clause (to-clause !-o-clause-i))

(define (to-clause-case clause-i)
  (lambda (head tail rule-case)
    (clause-i head tail rule-case)))
(define !-o-clause-case (to-clause-case !-o-clause-i))

(define (to-debug-clause-case full-clause-i)
  (lambda (head tail rule-case)
    (conde
      ((full-clause-i head tail rule-case))
      ((== tail '())
       (== rule-case '())
       (conde
         ((fresh (binding rho)
            (== `(lookupo ,binding ,rho) head)
            (lookupo binding rho)))
         ((fresh (x y)
            (== `(== ,x ,y) head)
            (== x y))))))))
(define !-o-debug-clause-case (to-debug-clause-case !-o-full-clause-i))
