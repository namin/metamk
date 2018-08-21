(load "mk2.scm")
(load "utils.scm")
(load "vanilla.scm")
(load "meta.scm")
(load "ex-evalo.scm")
(load "test-check.scm")

(test-check "evalo-base"
  (run* (q)
    (evalo
     '(letrec
          ((append
            (lambda (xs ys)
              (if (null? xs)
                  ys
                  (cons (car xs)
                        (append (cdr xs) ys))))))
        (append '(a b) '(c d e)))
     q))
  '((a b c d e)))

(test-check "evalo-vanilla"
  (run* (q)
    ((vanilla evalo-clause)
     `(evalo
       (letrec
           ((append
             (lambda (xs ys)
               (if (null? xs)
                   ys
                   (cons (car xs)
                         (append (cdr xs) ys))))))
         (append '(a b) '(c d e)))
       ,q)))
  '((a b c d e)))
