(define (is-reified-var c)
  (let ((s (string->list (symbol->string c))))
    (and
     (not (null? s)) (not (null? (cdr s))) (not (null? (cddr s)))
     (eq? (car s) #\_) (and (eq? (cadr s) #\.) (string->number (list->string (cddr s)))))))

(define (make-reified-var n)
  (string->symbol (list->string (cons #\_ (cons #\. (string->list (number->string n)))))))

(define (find-vars n m c)
  (cond
    ((null? c) m)
    ((symbol? c)
     (let ((x (is-reified-var c)))
       (if (and x (not (assq c m)))
           (cons (cons c (make-reified-var (+ n x))) m)
           m)))
    ((pair? c)
     (find-vars n (find-vars n m (car c)) (cdr c)))
    (else m)))

(define (replace-vars m c)
  (cond
    ((null? c) (list 'quote '()))
    ((symbol? c)
     (let ((r (assq c m)))
       (if r (cdr r) (list 'quote c))))
    ((pair? c)
     (list 'cons (replace-vars m (car c)) (replace-vars m (cdr c))))
    (else (list 'quote c))))

(define (reify-clause n c)
  (let ((m (find-vars n '() c)))
    (list (+ n (length m)) m (replace-vars m c))))

(define (iter-reify-clause ps)
  (let loop ((n 0)
             (ps ps)
             (vs '())
             (cs '()))
    (if (null? ps) (list vs cs)
        (let ((r (reify-clause n (car ps))))
          (let ((n (car r))
                (m (cadr r))
                (c (caddr r)))
            (loop n (cdr ps) (append (map cdr m) vs) (cons c cs)))))))

(define (reify-prog run-result)
  (let ((r (iter-reify-clause run-result)))
    (let ((vs (car r))
          (cs (cadr r)))
      (lambda (p)
        (list 'fresh vs
              (list '== p (cons 'list (reverse cs))))))))

(define (refl-prog pclause)
  (let ((program-obj
         (run* (q)
           (fresh (a b)
             (== q  (list '<- a b))
             (pclause a b)))))
    (let ((q (reify-prog program-obj)))
      (eval `(lambda (p) ,(q 'p))))))
