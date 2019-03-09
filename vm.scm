(include "match.scm")

(define code-stream (let ((code (list 'main))) (cons code code)))

(define (gen . instrs)
  (set-cdr! (car code-stream) instrs)
  (set-car! code-stream (last-pair instrs)))

(define (dest label) (member label (cdr code-stream)))

(define (patch-jump instr label) (set-car! (cdr instr) label))

(define (start-execution arg) (exec (dest 'main) `(,arg)))

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
    ((stub ,thunk) (set-car! pc `(jump ,(thunk))) (exec pc s))
    ((add)         (exec (cdr pc) (cons (+ (cadr s) (car s)) (cddr s))))
    ((lt)          (exec (cdr pc) (cons (< (cadr s) (car s)) (cddr s))))
    ((println)     (begin (println (car s)) (exec (cdr pc) (cdr s))))
    (,label        (exec (cdr pc) s))))

;; test

; (gen `(push-lit 42)
;      `(push-loc 0)
;      `(add)
;      `(println)
;      `(stub ,(lambda () (println "stub, jumping to bar") 'bar))
;      `foo
;      `(push-lit 11)
;      `(println)
;      `(halt)
;      `bar
;      `(push-lit 22)
;      `(println)
;      `(halt))
;
; (start-execution)
