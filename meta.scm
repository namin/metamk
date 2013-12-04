(load "mk.scm")

(define-syntax defrel
  (syntax-rules ()
    ((_ ((cid* r* ...) ...) (id a* ...) body)
     (begin
       (define (id a* ...)
         body)
       (define (cid* head tail)
         (define (cdr-down tail)
           (if (var? tail)
               tail
               (cdr-down (cdr tail))))
         (define (snoco tail el)
           (project (tail)
             (fresh (new-tail)
               (== (cdr-down tail) (cons el new-tail)))))
         (define r*
           (lambda args
             (snoco tail `(r* . ,args))))
         ...
         
         (fresh ()
           (fresh (a* ...)
             (== head `(id ,a* ...))
             body)
           (project (tail)
             (== (cdr-down tail) '()))))
         ...
       (void)))))

