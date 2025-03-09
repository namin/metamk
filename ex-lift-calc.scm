;; Lift domain for EBG

(define (calculate x xn)
  (conda
    [(fresh [yn1 yn2 ys]
       (== [list ys '+ yn1] x)
       (calculate ys yn2)
       (project [yn1 yn2]
         (== (+ yn1 yn2) xn)))]
    [(project [x]
       (== x xn))]))

(define (lift-domain-clause a b)
  (conde
    [(fresh [level goal-level moves distance]
       (== a [list 'go level goal-level moves])
       (== b [list
              [list 'move-list moves distance]
              [list distance '=:= goal-level '- level]]))]
    [(== a [list 'move-list '() 0])
     (== b '())]
    [(fresh [move1 moves distance distance1]
       (== a [list 'move-list (lcons move1 moves) [list distance '+ distance1]])
       (== b [list
              [list 'move move1 distance1]
              [list 'move-list moves distance]]))]
    [(== a [list 'move 'up 1])
     (== b '())]
    [(== a [list 'move 'down -1])
     (== b '())]
    [(fresh [x xn yn zn]
       (== a [list x '=:= yn '- zn])
       (calculate x xn)
       (project [xn yn zn]
         (== xn (- yn zn))))]))

(define (lift-domain-operational goal isop)
  (conda
    [(fresh [distance goal-level level]
       (== goal [list distance '=:= goal-level '- level]))
     (== isop true)]
    [(== isop false)]))

(define ex-lift-domain-solver (vanilla lift-domain-clause))
(define ex-lift-domain-ebg (ebg-for lift-domain-clause lift-domain-operational))
