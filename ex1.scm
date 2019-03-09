(include "vm.scm")
(include "comp.scm")

(define (gen-expr expr env)

  (match expr

    (,c when (constant? c)
      (gen `(push-lit ,c)))

    (,v when (variable? v)
      (gen `(push-loc ,(lookup v env))))

    ((let ,v ,E1 ,body) when (variable? v)
      (gen-expr E1 env)
      (gen-expr body (cons v env))
      (gen '(swap))
      (gen '(pop)))

    ((if ,E1 ,E2 ,E3)
      (gen-expr E1 env)
      (let ((label-join (make-label 'label_join))
            (label-else (make-label 'label_else)))
        (gen `(iffalse ,label-else))
        (gen-expr E2 env)
        (gen `(jump ,label-join))
        (gen label-else)
        (gen-expr E3 env)
        (gen label-join)))

    (else (error "Unknown expr"))))

(define (gen-program body)
  (gen-expr body '(arg))
  (gen '(println))
  (gen '(halt)))

(gen-program '(if arg 10 20))
(println "## Stream before execution: ")
(pp code-stream)
(println "## Execution result: ")
(start-execution 0)
(println "## Stream after execution: ")
(pp code-stream)
