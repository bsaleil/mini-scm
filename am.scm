(include "match.scm")

(define code-stream (list 'main))
(define code-stream-end code-stream)

(define (gen . instrs)
  (set-cdr! code-stream-end instrs)
  (set! code-stream-end (last-pair instrs)))

(define (dest label) (member label code-stream))

(define (start-execution label stack) (exec (dest label) stack))

(define (exec pc s)
  (match (car pc)
    ((swap)        (exec (cdr pc) (cons (cadr s) (cons (car s) (cddr s)))))
    ((pop)         (exec (cdr pc) (cdr s)))
    ((halt)        s)
    ((push-lit ,v) (exec (cdr pc) (cons v s)))
    ((push-loc ,i) (exec (cdr pc) (cons (list-ref s i) s)))
    ((jump ,d)     (exec (dest d) s))
    ((iffalse ,d)  (exec (if (eqv? (car s) #f) (dest d) (cdr pc)) (cdr s)))
    ((ifnum ,d)    (exec (if (number? (car s)) (dest d) (cdr pc)) (cdr s)))
    ((callback ,t) (set-car! pc `(jump ,(t))) (exec pc s))
    ((println)     (println (car s)) (exec (cdr pc) (cons #f (cdr s))))
    (,label        (exec (cdr pc) s))))

;; test

; (gen `(push-lit 42)
;      `(push-loc 0)
;      `(add)
;      `(println)
;      `(callback ,(lambda () (println "callback, jumping to bar") 'bar))
;      `foo
;      `(push-lit 11)
;      `(println)
;      `(halt)
;      `bar
;      `(push-lit 22)
;      `(println)
;      `(halt))
;
; (start-execution 'main '())
