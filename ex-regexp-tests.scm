(load "mk2.scm")
(load "utils.scm")
(load "vanilla.scm")
(load "meta.scm")
(load "ex-regexp.scm")
(load "test-check.scm")

(test-check "regexp-matcho-1"
  (run 6 (q)
    (regexp-matcho '(seq foo (rep bar))
                  q
                  regexp-BLANK))
  '((foo)
    (foo bar)
    (foo bar bar)
    (foo bar bar bar)
    (foo bar bar bar bar)
    (foo bar bar bar bar bar)))

(test-check "regexp-matcho-vanilla-1"
  (run 6 (q)
    ((vanilla regexp-clause)
     `(regexp-matcho
       (seq foo (rep bar))
       ,q
       ,regexp-BLANK)))
  '((foo)
    (foo bar)
    (foo bar bar)
    (foo bar bar bar)
    (foo bar bar bar bar)
    (foo bar bar bar bar bar)))

