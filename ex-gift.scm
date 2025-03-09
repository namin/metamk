;; Gift domain for EBG

(define (gift-domain-clause a b)
  (conde
    [(conde
       [(== a [list 'likes 'john 'annie])]
       [(== a [list 'likes 'annie 'john])]
       [(== a [list 'likes 'john 'chocolate])]
       [(== a [list 'needs 'annie 'tennis-racket])]
       [(== a [list 'sad 'john])])
      (== b '())]
    [(fresh [person1 person2 gift]
       (== a [list 'gives person1 person2 gift])
       (== b [list
              [list 'likes person1 person2]
              [list 'would-please gift person2]]))]
    [(fresh [person1 person2 gift]
       (== a [list 'gives person1 person2 gift])
       (== b [list
              [list 'feels-sorry-for person1 person2]
              [list 'would-comfort gift person2]]))]
    [(fresh [gift person]
       (== a [list 'would-please gift person])
       (== b [list [list 'needs person gift]]))]
    [(fresh [gift person]
       (== a [list 'would-comfort gift person])
       (== b [list [list 'likes person gift]]))]
    [(fresh [person1 person2]
       (== a [list 'feels-sorry-for person1 person2])
       (== b [list
              [list 'likes person1 person2]
              [list 'sad person2]]))]
    [(fresh [person]
       (== a [list 'feels-sorry-for person person])
       (== b [list [list 'sad person]]))]))

(define (gift-domain-operational goal isop)
  (conda
    [(fresh [x y]
       (conde
         [(== goal [list 'likes x y])]
         [(== goal [list 'needs x y])]
         [(== goal [list 'sad x])]))
      (== isop true)]
    [(== isop false)]))

(define ex-gift-domain-solver (vanilla gift-domain-clause))
(define ex-gift-domain-ebg (ebg-for gift-domain-clause gift-domain-operational))
