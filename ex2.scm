(include "am.scm")
(include "comp.scm")

;;-----------------------------------------------------------------------------
;; Lazy generation

(define (gen-expr-lazily expr env cont)
  (gen `(callback ,(lambda ()
                     (let ((label (gensym 'lazily_generated)))
                       (gen label)
                       (gen-expr expr env cont)
                       label)))))

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
                     (gen `(iffalse ,else))
                     (gen-expr-lazily E2 env cont)
                     (gen else)
                     (gen-expr-lazily E3 env cont)))))
       (gen-expr E1 env test)))

    (else (error "Unknown expr"))))

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
