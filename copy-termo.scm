(define copy-termo
  (lambda (u v)
    (project (u)
      (== (walk* u (build-s u '())) v))))

(define build-s
  (lambda (u s)
    (cond
      ((var? u) (if (assq u s) s (cons (cons u (var 'ignore)) s)))
      ((pair? u) (build-s (cdr u) (build-s (car u) s)))
      (else s))))
