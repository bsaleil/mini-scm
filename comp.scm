(define (index-of id env)
  (let loop ((env env) (idx 0))
    (cond ((null? env) (error "Unknown identifier" id))
          ((eq? (car env) id) idx)
          (else (loop (cdr env) (+ idx 1))))))

(define lab-count 0)
(define (gensym lab)
  (set! lab-count (+ lab-count 1))
  (string->symbol
   (string-append (symbol->string lab)
                  "_"
                  (number->string lab-count))))
