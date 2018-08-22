(load "mk2.scm")
(load "utils.scm")
(load "vanilla.scm")
(load "meta.scm")
(load "ex-cbv-lc.scm")
(load "preduce.scm")
(load "test-check.scm")

(test-check "1 CBV LC"
  (run* (q) (eval-expo '((lambda (x) (x x)) (lambda (x) x)) '() q))
  '((closure x x ())))

(test-check "2 CBV LC"
  (run 10 (q) (eval-expo q '() '(closure x x ())))
  '((lambda (x) x)
    (((lambda (_.0) _.0) (lambda (x) x)) (sym _.0))
    (((lambda (_.0) (_.0 _.0)) (lambda (x) x)) (sym _.0))
    (((lambda (_.0) _.0) ((lambda (_.1) _.1) (lambda (x) x)))
     (sym _.0 _.1))
    ((((lambda (_.0) _.0) (lambda (_.1) _.1)) (lambda (x) x))
     (sym _.0 _.1))
    (((lambda (_.0) ((lambda (_.1) _.1) _.0)) (lambda (x) x))
     (=/= ((_.0 lambda)))
     (sym _.0 _.1))
    (((lambda (_.0) (_.0 _.0))
      ((lambda (_.1) _.1) (lambda (x) x)))
     (sym _.0 _.1))
    (((lambda (_.0) ((lambda (_.1) _.0) _.0)) (lambda (x) x))
     (=/= ((_.0 _.1)) ((_.0 lambda)))
     (sym _.0 _.1))
    (((lambda (_.0) (_.0 _.0))
      ((lambda (_.1) (lambda (_.2) _.1)) (lambda (x) x)))
     (=/= ((_.1 _.2)) ((_.1 lambda)))
     (sym _.0 _.1 _.2))
    (((lambda (_.0) ((lambda (_.1) _.0) (lambda (_.2) _.3)))
      (lambda (x) x))
     (=/= ((_.0 _.1)) ((_.0 lambda)))
     (sym _.0 _.1 _.2))))

(test-check "meta 1 CBV LC"
  (run* (q) ((vanilla lc-clause) `(eval-expo ((lambda (x) (x x)) (lambda (x) x)) () ,q)))
  '((closure x x ())))

(define lc-program-obj
  (run* (q)
    (fresh (a b)
      (== q  (list '<- a b))
      (lc-pclause a b))))
(define (lc-program p)
  (fresh (a_0 a_1
              b_0 b_1 b_2 b_3 b_4 b_5 b_6 b_7 b_8
              c_0 c_1 c_2
              d_0 d_1 d_2 d_3 d_4
              e_0 e_1 e_2 e_3 e_4
              f_0 f_1 f_2 f_3 f_4 f_5
              g_0 g_1 g_2 g_3 g_4 g_5)
    (== p
     (list
      `(<- (not-in-envo ,a_0 ,a_1) ((== () ,a_1)))
      `(<- (eval-expo ,b_0 ,b_1 ,b_2)
           ((== (,b_3 ,b_4) ,b_0)
            (eval-expo ,b_3 ,b_1 (closure ,b_5 ,b_6 ,b_7))
            (eval-expo ,b_4 ,b_1 ,b_8)
            (eval-expo ,b_6 ((,b_5 . ,b_8) . ,b_7) ,b_2)))
      `(<- (eval-expo ,c_0 ,c_1 ,c_2)
           ((symbolo ,c_0) (lookupo ,c_0 ,c_1 ,c_2)))
      `(<- (eval-expo ,d_0 ,d_1 ,d_2)
           ((== (lambda (,d_3) ,d_4) ,d_0)
            (symbolo ,d_3)
            (l== (closure ,d_3 ,d_4 ,d_1) ,d_2)
            (not-in-envo lambda ,d_1)))
      `(<- (not-in-envo ,e_0 ,e_1)
           ((== ((,e_2 . ,e_3) . ,e_4) ,e_1)
            (=/= ,e_2 ,e_0)
            (not-in-envo ,e_0 ,e_4)))
      `(<- (lookupo ,f_0 ,f_1 ,f_2)
           ((== ((,f_3 . ,f_4) . ,f_5) ,f_1) (== ,f_3 ,f_0) (== ,f_4 ,f_2)))
      `(<- (lookupo ,g_0 ,g_1 ,g_2)
           ((== ((,g_3 . ,g_4) . ,g_5) ,g_1)
            (=/= ,g_3 ,g_0)
            (lookupo ,g_0 ,g_5 ,g_2)))))))

;; TODO: need to figure out fold/unfold
(define (should-fold g r)
  (fresh (x y)
    (conde
      ((== g `(l== ,x ,y))
       (== r `(== ,x ,y))))))

(define (should-unfold g)
  (absento 'l== g))

;; TODO: what do we want?
(run 1 (q)
  (fresh (p)
    (lc-program p)
    ((preduce* lc-clause)
     p
     q)))
