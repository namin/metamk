(define-syntax fresh-if-needed
  (syntax-rules ()
    ((_ () g)
     g)
    ((_ (x* ...) g)
     (fresh (x* ...) g))))

;; Key idea:
;; For each relation to reify, the macro lexically redefines it
;; to add a representation of the call to the tail
;; (instead of executing the underlying call).
(define-syntax define-rel
  (syntax-rules ()
    ((_ (id a* ...) ((cid* r* ...) ...) (x* ...) body)
     (begin
       (define (id a* ...) (fresh-if-needed (x* ...) body))
       (define (cid* head tail x* ...)
         (define (cdr-down tail)
           (if (var? tail)
               tail
               (cdr-down (cdr tail))))
         (define (snoco tail el)
           (project (tail)
             (fresh (new-tail)
               (== (cdr-down tail) (cons el new-tail)))))
         (define (closeo tail)
           (project (tail)
             (== (cdr-down tail) '())))
         (define r*
           (lambda args
             (snoco tail `(r* . ,args))))
         ...
         (fresh ()
           (fresh (a* ...)
             (== head `(id ,a* ...))
             body)
           (closeo tail)))
         ...
       (void)))))
