;; Section 25.6 Explanation-based generalization
;; from the Art of Prolog
;; see also https://github.com/namin/logically/blob/master/src/logically/ai/meta/ebg.clj

(load "appendo.scm")
(load "copy-termo.scm")

(define (conso a b ab)
  (== (cons a b) ab))

(define (lcons a b)
  (cons a b))

(define true #t)

(define false #f)

(define (ebg-for* clause operational)
  (define solver (vanilla clause))
  (define (ebg goals gengoals conditions)
    (conde
     [(== goals '())
      (== gengoals '())
      (== conditions '())]
     [(fresh [goal gs gengoal gengs hcs tcs]
             (conso goal gs goals)
             (conso gengoal gengs gengoals)
             (conde
              [(operational gengoal true)
               (== hcs [list gengoal])
               (solver goal)]
              [(operational goal false)
               (fresh [cgoal cbody body genbody]
                      (clause gengoal genbody)
                      (copy-termo (lcons gengoal genbody) (lcons goal body))
                      (ebg body genbody hcs))])
             (appendo hcs tcs conditions)
             (ebg gs gengs tcs))]))
  ebg)

(define (ebg-for clause operational)
  (let ((ebg* (ebg-for* clause operational)))
    (lambda (goal gengoal conditions)
      (ebg* (list goal) (list gengoal) conditions))))
