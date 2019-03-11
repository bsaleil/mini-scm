(include "am.scm")
(include "comp.scm")

;;-----------------------------------------------------------------------------
;; Lazy generation

(define (gen-stub thunk)
  (let ((label (gensym 'stub)))
    (set! code-stream (cons label (cons `(callback ,thunk) code-stream)))
    label))

(define (gen-expr-lazily jump expr env cont)
  (let ((instr `(,jump ???)))
    (set-car! (cdr instr)
              (gen-stub (lambda ()
                          (let ((label (gensym 'lazily_generated)))
                            (set-car! (cdr instr) label)
                            (gen label)
                            (gen-expr expr env cont)
                            label))))
    (gen instr)))

;;-----------------------------------------------------------------------------

(define (gen-expr expr env cont)

  (match expr

    (,c when (constant? c)
     (gen `(push-lit ,c))
     (cont))

    (,v when (variable? v)
     (gen `(push-loc ,(index-of v env)))
     (cont))

    ((println ,E)
     (gen-expr E env (lambda () (gen `(println)) (cont))))

    ((let ,v ,E1 ,E2) when (variable? v)
     (let* ((exit (lambda ()
                    (gen `(swap) `(pop))
                    (cont)))
            (body (lambda ()
                    (gen-expr E2 (cons v env) exit))))
       (gen-expr E1 env body)))

    ((if ,E1 ,E2 ,E3)
     (let ((test (lambda ()
                   (let ((else (gensym 'else)))
                     (gen-expr-lazily 'iffalse E3 env cont)
                     (gen-expr-lazily 'jump E2 env cont)))))
       (gen-expr E1 env test)))

    (else (error "Unknown expression"))))

(define (gen-program body)
  (gen-expr body '(arg) (lambda () (gen `(halt)))))

(gen-program '(println (if arg 10 20)))
(println "## Stream before execution: ")
(pp code-stream)
(println "## Execution result: ")
(start-execution 'main (list 0))
(println "## Stream after execution with arg == 0: ")
(pp code-stream)
(start-execution 'main (list #f))
(println "## Stream after execution with arg == #f: ")
(pp code-stream)
