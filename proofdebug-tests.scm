(load "proofdebug.scm")
(load "ex-peano.scm")
(load "ex-stlc.scm")
(load "test-check.scm")

(test-check "stlc-proofdebug-ok"
  (run* (q)
    (fresh (term typ proof ok)
      (== term '(lambda (x) x))
      (== q `(,term ,typ ,proof, ok))
      ((proofdebug !-o-clause-case)
       `(!-o () ,term ,typ)
       proof
       ok)))
  '(((lambda (x) x)
     (_.0 -> _.0)
     (((!-o () (lambda (x) x) (_.0 -> _.0))
       abs
       <--
       (((!-o ((x : _.0)) x _.0) var <-- ()))))
     #t)))

(test-check "stlc-proofdebug-full-ok"
  (run* (q)
    (fresh (term typ proof ok)
      (== term '(lambda (x) x))
      (== q `(,term ,typ ,proof, ok))
      ((proofdebug !-o-debug-clause-case)
       `(!-o () ,term ,typ)
       proof
       ok)))
  '(((lambda (x) x)
   (_.0 -> _.0)
   (((!-o () (lambda (x) x) (_.0 -> _.0))
      abs
      <--
      (((!-o ((x : _.0)) x _.0)
         var
         <--
         (((lookupo (x : _.0) ((x : _.0))) () <-- ()))))))
   #t)))

(test-check "stlc-proofdebug-unbound-var"
  (run* (q)
    (fresh (term typ proof ok)
      (== term '(lambda (x) (x y)))
      (== q `(,term ,typ ,proof, ok))
      ((proofdebug !-o-debug-clause-case)
       `(!-o () ,term ,typ)
       proof
       ok)))
  '(((lambda (x) (x y))
     ((_.0 -> _.1) -> _.1)
     (((!-o () (lambda (x) (x y)) ((_.0 -> _.1) -> _.1))
       abs
       <--
       (((!-o ((x : (_.0 -> _.1))) (x y) _.1)
         app
         <--
         (((!-o ((x : (_.0 -> _.1))) x (_.0 -> _.1))
           var
           <--
           (((lookupo (x : (_.0 -> _.1)) ((x : (_.0 -> _.1))))
             ()
             <--
             ())))
          ((!-o ((x : (_.0 -> _.1))) y _.0)
           var
           <--
           (((lookupo (y : _.0) ((x : (_.0 -> _.1)))) error))))))))
     #f)))
