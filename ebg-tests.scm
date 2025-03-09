(load "mk.scm")
(load "utils.scm")
(load "vanilla.scm")
(load "meta.scm")
(load "matche.scm")
(load "ebg.scm")
(load "ex-gift.scm")
(load "ex-lift-calc.scm")
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

(test-check
 "test-ex-lift-domain-1"
 (run 1 [moves]
      (ex-lift-domain-solver [list 'go 3 6 moves]))
 '((up up up)))

(test-check
 "test-ex-lift-domain-2"
 (run 1 [q]
      (fresh [moves level1 level2 gen-moves conditions]
             (ex-lift-domain-ebg [list 'go 3 6 moves] [list 'go level1 level2 gen-moves] conditions)
             (== q [list moves gen-moves level1 level2 conditions])))
 '([(up up up) (up up up) _.0 _.1 ([[[[0 + 1] + 1] + 1] =:= _.1 - _.0])]))


