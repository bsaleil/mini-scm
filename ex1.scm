(include "am.scm")
(include "comp.scm")

(define (gen-expr expr env)

  (match expr

    (,c when (constant? c)
     (gen `(push-lit ,c)))

    (,v when (variable? v)
     (gen `(push-loc ,(index-of v env))))

    ((println ,E)
     (gen-expr E env)
     (gen `(println)))

    ((let ,v ,E1 ,E2) when (variable? v)
     (gen-expr E1 env)
     (gen-expr E2 (cons v env))
     (gen `(swap) `(pop)))

    ((if ,E1 ,E2 ,E3)
     (gen-expr E1 env)
     (let ((join (gensym 'join))
           (else (gensym 'else)))
       (gen `(iffalse ,else))
       (gen-expr E2 env)
       (gen `(jump ,join))
       (gen else)
       (gen-expr E3 env)
       (gen join)))

    (else (error "Unknown expr"))))

(define (gen-program body)
  (gen-expr body '(arg))
  (gen `(halt)))

(gen-program '(println (if arg 10 20)))
(println "## Stream before execution: ")
(pp code-stream)
(println "## Execution result: ")
(start-execution 'main (list 0))
(println "## Stream after execution with arg == 0: ")
(pp code-stream)
