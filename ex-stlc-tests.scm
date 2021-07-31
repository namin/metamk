(load "mk.scm")
(load "meta.scm")
(load "matche.scm")
(load "ex-stlc.scm")
(load "test-check.scm")

(test-check "slides-!-o-type-inhabiter"
  (run 3 (q) (eigen (alpha) (!-o '() q `(,alpha -> ,alpha))))
  '(((lambda (_.0) _.0) (sym _.0))
    ((lambda (_.0) ((lambda (_.1) _.1) _.0)) (sym _.0 _.1))
    (((lambda (_.0) _.0) (lambda (_.1) _.1)) (sym _.0 _.1))))

(test-check "slides-!-o-self-app-failure"
  (run 1 (q) (!-o '() '(lambda (x) (x x)) q))
  '())

(test-check "!-o-identity-1"
  (run 10 (q) (eigen (alpha) (!-o '() q `(,alpha -> ,alpha))))
  '(((lambda (_.0) _.0) (sym _.0))
    ((lambda (_.0) ((lambda (_.1) _.1) _.0)) (sym _.0 _.1))
    (((lambda (_.0) _.0) (lambda (_.1) _.1)) (sym _.0 _.1))
    ((lambda (_.0) ((lambda (_.1) _.0) _.0)) (=/= ((_.0 _.1)))
     (sym _.0))
    ((lambda (_.0)
       ((lambda (_.1) _.1) ((lambda (_.2) _.2) _.0)))
     (sym _.0 _.1 _.2))
    ((lambda (_.0) ((lambda (_.1) _.0) (lambda (_.2) _.2)))
     (=/= ((_.0 _.1))) (sym _.0 _.2))
    (((lambda (_.0) (lambda (_.1) _.1)) (lambda (_.2) _.2))
     (sym _.1 _.2))
    ((lambda (_.0) ((lambda (_.1) _.0) (lambda (_.2) _.0)))
     (=/= ((_.0 _.1)) ((_.0 _.2))) (sym _.0))
    ((lambda (_.0)
       ((lambda (_.1) _.1) ((lambda (_.2) _.0) _.0)))
     (=/= ((_.0 _.2))) (sym _.0 _.1))
    (((lambda (_.0) _.0)
      (lambda (_.1) ((lambda (_.2) _.2) _.1)))
     (sym _.0 _.1 _.2))))

(test-check "!-o-identity-2"
  ((eval
    (caar (reverse (run 1000 (q) (eigen (alpha) (!-o '() q `(,alpha -> ,alpha))))))
    (environment '(rnrs)))
   5)
  5)

;;; disabled because extra fresh causing different result
(test-disable "!-o-1"
  (run 10 (q) (fresh (e T) (!-o '() e T) (== `(,e ,T) q)))
  '((((lambda (_.0) _.0) (_.1 -> _.1)) (sym _.0))
    (((lambda (_.0) (lambda (_.1) _.1)) (_.2 -> (_.3 -> _.3))) (sym _.1))
    (((lambda (_.0) (lambda (_.1) _.0)) (_.2 -> (_.3 -> _.2))) (=/= ((_.0 _.1))) (sym _.0))
    ((((lambda (_.0) _.0) (lambda (_.1) _.1)) (_.2 -> _.2)) (sym _.0 _.1))
    (((lambda (_.0) (lambda (_.1) (lambda (_.2) _.2))) (_.3 -> (_.4 -> (_.5 -> _.5)))) (sym _.2))
    (((lambda (_.0) (lambda (_.1) (lambda (_.2) _.1))) (_.3 -> (_.4 -> (_.5 -> _.4)))) (=/= ((_.1 _.2))) (sym _.1))
    (((lambda (_.0) (lambda (_.1) (lambda (_.2) _.0))) (_.3 -> (_.4 -> (_.5 -> _.3)))) (=/= ((_.0 _.1)) ((_.0 _.2))) (sym _.0))
    (((lambda (_.0) (_.0 (lambda (_.1) _.1))) (((_.2 -> _.2) -> _.3) -> _.3)) (sym _.0 _.1))
    ((((lambda (_.0) _.0) (lambda (_.1) (lambda (_.2) _.2))) (_.3 -> (_.4 -> _.4))) (sym _.0 _.2))
    (((lambda (_.0) ((lambda (_.1) _.1) _.0)) (_.2 -> _.2)) (sym _.0 _.1))))
