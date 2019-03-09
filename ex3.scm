(include "vm.scm")
(include "comp.scm")

;;-----------------------------------------------------------------------------
;; Context

(define (ctx-push ctx t) (cons t ctx))
(define ctx-top car)
(define ctx-pop cdr)
(define ctx-get list-ref)

;;-----------------------------------------------------------------------------
;; Versioning

(define version-table '())
(define (gen-cont cont ctx)

  (define (add-version label)
    (let ((version (cons ctx label))
          (versions (assq cont version-table)))
      (if versions
          (set-cdr! versions (cons version (cdr versions)))
          (set! version-table (cons (cons cont (list version)) version-table)))))

  (define (gen-version)
    (let ((label (make-label 'version)))
      (gen label)
      (cont ctx)
      (add-version label)
      label))

  (define (get-version)
    (let ((versions (assq cont version-table)))
      (and versions
           (let ((version (assoc ctx (cdr versions))))
             (and version
                  (cdr version))))))

  (let ((label (get-version)))
    (or label
        (gen-version))))

;;-----------------------------------------------------------------------------
;; Stub

(define (make-stub expr env cont ctx)
  (lambda ()
    (let ((label (make-label 'lazy)))
      (gen label)
      (gen-expr expr env cont ctx)
      label)))

;;-----------------------------------------------------------------------------

(define (gen-expr expr env cont ctx)

  (match expr

    (,c when (constant? c)
      (gen `(push-lit ,c))
      (gen-cont cont (ctx-push ctx (if (boolean? c) 'bool 'fix))))

    (,v when (variable? v)
      (let ((idx (lookup v env)))
        (gen `(push-loc ,idx))
        (gen-cont cont (ctx-push ctx (ctx-get ctx idx)))))

    ((let ,v ,E1 ,body) when (variable? v)
      (let* ((last (lambda (ctx)
                     (gen '(swap))
                     (gen '(pop))
                     (let ((type (ctx-top ctx)))
                       (gen-cont cont (ctx-push (ctx-pop (ctx-pop ctx)) type)))))
             (cont (lambda (ctx)
                     (gen-expr body (cons v env) last ctx))))
        (gen-expr E1 env cont ctx)))

    ((if ,E1 ,E2 ,E3)
       (let ((cont
               (lambda (ctx)
                 (let ((type (ctx-get ctx 0)))
                   (if (or (eq? type 'bool)
                           (eq? type 'unknown))
                       ;; If propagated type is 'bool or 'unknown, generate dispatch code
                       (let ((lelse (make-label 'label-else)))
                         (gen `(iffalse ,lelse))
                         (gen `(stub ,(make-stub E3 env cont (ctx-pop ctx))))
                         (gen lelse)
                         (gen `(stub ,(make-stub E2 env cont (ctx-pop ctx)))))
                       ;; Else, generate the then branch inline
                       (gen-expr E2 env cont (ctx-pop ctx)))))))
         (gen-expr E1 env cont ctx)))

    (else (error "Unknown expr"))))

(define (gen-program expr)
  (lambda (ctx)
    (gen-expr
      expr
      '(arg)
      (lambda (ctx)
        (gen '(println))
        (gen '(halt)))
      ctx)))

(let ((first (gen-program '(if arg 10 20))))
  ;;
  (println "## Generate code with fixnum arg")
  (let ((label (gen-cont first '(fix))))
    (println " ## Generated version is " label)
    (println " ## Stream before execution with arg == 10:")
    (print   "    ") (pp (cdr code-stream))
    (exec (dest label) '(10))
    (println " ## Stream after execution with arg == 10:")
    (print   "    ") (pp (cdr code-stream)))
  ;;
  (println "## Generate code with boolean arg")
  (let ((label (gen-cont first '(bool))))
    (println " ## Generated version is " label)
    (println " ## Stream before execution with arg == #f:")
    (print   "    ") (pp (cdr code-stream))
    (exec (dest label) '(#f))
    (println " ## Stream after execution with arg == #f:")
    (print   "    ") (pp (cdr code-stream))))
