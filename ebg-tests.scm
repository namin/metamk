(load "mk.scm")
(load "utils.scm")
(load "vanilla.scm")
(load "meta.scm")
(load "matche.scm")
(load "ebg.scm")
(load "ex-gift.scm")
(load "test-check.scm")

(test-check
 "test-ex-gift-domain-free"
 (run* [q]
       (ex-gift-domain-solver [list 'gives 'john 'john 'chocolate]))
 '(_.0))

(test-check
 "test-ex-gift-domain-nil"
 (run* [q]
       (ex-gift-domain-solver [list 'gives 'john 'annie 'chocolate]))
 '())

(test-check
 "test-ex-gift-domain-gen"
 (run 1 [q]
      (fresh [x y z cs]
             (ex-gift-domain-ebg [list 'gives 'john 'john 'chocolate] [list 'gives x y z] cs)
             (== q [list x y z cs])))
 '([_.0 _.0 _.1 ([sad _.0] [likes _.0 _.1])]))



