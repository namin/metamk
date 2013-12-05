(load "prooftree.scm")
(load "ex-peano.scm")
(load "ex-stlc.scm")
(load "test-check.scm")

(test-check "pluso-prooftree"
  (run 3 (q)
    (fresh (a b c proof)
      (== q `((,a ,b ,c) ,proof))
      ((prooftree pluso-clause-case)
       `(pluso ,a ,b ,c) proof)))
  '(((z _.0 _.0) (((pluso z _.0 _.0) () <-- ())))
    (((s z) _.0 (s _.0))
     (((pluso (s z) _.0 (s _.0))
       () <--
       (((pluso z _.0 _.0) ()  <-- ())))))
    (((s (s z)) _.0 (s (s _.0)))
     (((pluso (s (s z)) _.0 (s (s _.0)))
       ()  <--
       (((pluso (s z) _.0 (s _.0))
         () <--
         (((pluso z _.0 _.0) ()  <-- ())))))))))

(test-check "stlc-prooftree-no-eigen"
  (run 2 (q)
    (fresh (term proof)
      (== q `(,term ,proof))
      ((prooftree !-o-clause-case)
       `(!-o () ,term (A -> A))
       proof)))
  '((((lambda (_.0) _.0)
      (((!-o () (lambda (_.0) _.0) (A -> A))
        abs <--
        (((!-o ((_.0 : A)) _.0 A) var <-- ())))))
     (sym _.0))
    (((lambda (_.0) ((lambda (_.1) _.1) _.0))
      (((!-o () (lambda (_.0) ((lambda (_.1) _.1) _.0)) (A -> A))
        abs <--
        (((!-o ((_.0 : A)) ((lambda (_.1) _.1) _.0) A)
          app <--
          (((!-o ((_.0 : A)) (lambda (_.1) _.1) (A -> A))
            abs <--
            (((!-o ((_.1 : A) (_.0 : A)) _.1 A) var <-- ())))
           ((!-o ((_.0 : A)) _.0 A) var <-- ())))))))
     (sym _.0 _.1))))

(test-check "stlc-prooftree-if"
  (run* (q)
    (fresh (term ty proof)
      (== term '(lambda (x) (if x (lambda (y) x) (lambda (y) y))))
      (== q `(,term ,ty ,proof))
      ((prooftree !!-o-clause-case)
       `(!!-o () ,term ,ty)
       proof)))
  '(((lambda (x) (if x (lambda (y) x) (lambda (y) y)))
     (boolean -> (boolean -> boolean))
     (((!!-o
        ()
        (lambda (x) (if x (lambda (y) x) (lambda (y) y)))
        (boolean -> (boolean -> boolean)))
       abs
       <--
       (((!!-o
          ((x : boolean))
          (if x (lambda (y) x) (lambda (y) y))
          (boolean -> boolean))
         if
         <--
         (((!!-o ((x : boolean)) x boolean) var <-- ())
          ((!!-o ((x : boolean)) (lambda (y) x) (boolean -> boolean))
           abs
           <--
           (((!!-o ((y : boolean) (x : boolean)) x boolean)
             var
             <--
             ())))
          ((!!-o ((x : boolean)) (lambda (y) y) (boolean -> boolean))
           abs
           <--
           (((!!-o ((y : boolean) (x : boolean)) y boolean)
             var
             <--
             ())))))))))))
