;; Prolog-Style Meta-Programming miniKanren
;; miniKanren workshop, 2021
;; Nada Amin, Will Byrd, Tiark Rompf

(load "mk.scm")
(load "meta.scm")
(load "matche.scm")
(load "utils.scm")
(load "test-check.scm")

(define-rel
  (!-o gamma expr type) () (rule-case)
  (matche (expr type)

    ((,x ,T)
     (symbolo x)
     (lookupo `(,x : ,T) gamma)
     (== rule-case 'var))

    (((lambda (,x) ,e) (,T1 -> ,T2))
     (!-o `((,x : ,T1) . ,gamma) e T2)
     (== rule-case 'abs))

    (((,e1 ,e2) ,T)
     (fresh (T1)
       (!-o gamma e1 `(,T1 -> ,T))
       (!-o gamma e2 T1))
     (== rule-case 'app))))

(define (lookupo binding gamma)
  (matche (binding gamma)
    (((,x : ,v) ((,x : ,v) . ,_)))
    (((,x : ,v) ((,y : ,_) . ,gamma1)) (=/= x y)
     (lookupo binding gamma1))))

(run 1 (q)
  (!-o '() '(lambda (x) x) q))

(run 1 (q)
  (!-o '() '(lambda (x) (x x)) q))

;; cycler
(load "ex-path.scm")

(run 10 (q)
  (patho 'a q))

(load "cycler.scm")

(run* (q)
  (fresh (x y t)
    (== q `(,x ,y ,t))
    ((cycler patho-clause) `(patho ,x ,y) t)))
'((a b ((patho a b)))
  (a c ((patho b c) (patho a c)))
  (a a ((patho c a) (patho b a) (patho a a))))

'((a b ((patho a b))) (b c ((patho b c))) (c a ((patho c a)))
  (a c ((patho b c) (patho a c)))
  (c b ((patho a b) (patho c b)))
  (b a ((patho c a) (patho b a)))
  (a a ((patho c a) (patho b a) (patho a a)))
  (b b ((patho a b) (patho c b) (patho b b)))
  (c c ((patho b c) (patho a c) (patho c c))))

;; reification

(define (my-patho-clause head tail)
  (fresh (x y)
    (== head `(patho ,x ,y))
    (conde
      ((edgeo x y) (== tail '()))
      ((fresh (z)
         (edgeo x z)
         (== tail `((patho ,z ,y))))))))

(run* (q)
  (fresh (head tail)
    (== q `(to prove ,head prove ,tail))
    (my-patho-clause head tail)))
'((to prove (patho a b) prove ())
  (to prove (patho a _.0) prove ((patho b _.0)))
  (to prove (patho b c) prove ())
  (to prove (patho c a) prove ())
  (to prove (patho b _.0) prove ((patho c _.0)))
  (to prove (patho c _.0) prove ((patho a _.0))))
