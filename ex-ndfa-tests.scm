(load "mk2.scm")
(load "utils.scm")
(load "vanilla.scm")
(load "meta.scm")
(load "ex-ndfa.scm")
(load "test-check.scm")

(test-check "accept-base"
  (run 3 (xs)
    (accept xs))
  '(() (a b) (a b a b)))

(test-check "accept-vanilla"
  (run 3 (xs)
    ((vanilla ndfa-clause) `(accept ,xs)))
  '(() (a b) (a b a b)))
