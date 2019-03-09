(include "vm.scm")
(include "comp.scm")

;;-----------------------------------------------------------------------------
;; Stub

(define (make-stub expr env cont)
  (lambda ()
    (let ((label (make-label 'lazy)))
      (gen label)
      (gen-expr expr env cont)
      label)))

;;-----------------------------------------------------------------------------

(define (gen-expr expr env cont)

  (match expr

    (,c when (constant? c)
      (gen `(push-lit ,c))
      (cont))

    (,v when (variable? v)
      (gen `(push-loc ,(lookup v env)))
      (cont))

    ((let ,v ,E1 ,body) when (variable? v)
      (let* ((last (lambda ()
                     (gen '(swap))
                     (gen '(pop))
                     (cont)))
             (cont (lambda ()
                     (gen-expr body (cons v env) last))))
        (gen-expr E1 env cont)))

    ((if ,E1 ,E2 ,E3)
       (let ((cont
               (lambda ()
                 (let ((lelse (make-label 'label_else)))
                   (gen `(iffalse ,lelse))
                   (gen `(stub ,(make-stub E2 env cont)))
                   (gen lelse)
                   (gen `(stub ,(make-stub E3 env cont)))))))
         (gen-expr E1 env cont)))

    (else (error "Unknown expr"))))

(define (gen-program body)
  (gen-expr body '(arg) (lambda ()
                          (gen '(println))
                          (gen '(halt)))))

(gen-program '(if arg 10 20))
(println "## Stream before execution: ")
(pp code-stream)
(println "## Execution result: ")
(start-execution 0)
(println "## Stream after execution with arg == 0: ")
(pp code-stream)
(start-execution #f)
(println "## Stream after execution with arg == #f: ")
(pp code-stream)
