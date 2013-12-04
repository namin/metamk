(load "proofdebug.scm")
(load "ex-peano.scm")
(load "ex-stlc.scm")
(load "test-check.scm")

(test-check "stlc-prooftree-debug-ok"
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

(test-check "stlc-prooftree-full-debug-ok"
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
