(define (lookup id env)
  (let loop ((env env) (idx 0))
    (cond ((null? env) (error "Unknown identifier" id))
          ((eq? (car env) id) idx)
          (else (loop (cdr env) (+ idx 1))))))

(define sym-count 0)
(define (make-label sym)
  (set! sym-count (+ sym-count 1))
  (string->symbol
    (string-append (symbol->string sym)
                   "_"
                   (number->string sym-count))))
