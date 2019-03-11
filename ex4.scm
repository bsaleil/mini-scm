(include "am.scm")
(include "comp.scm")

;;-----------------------------------------------------------------------------
;; Context operations

(define (cpush ctx type) (cons type ctx))
(define ctop car)
(define cpop cdr)
(define cref list-ref)

;;-----------------------------------------------------------------------------
;; Versioning

(define versions '())

(define (get-cont-label cont ctx existing)

  (define (gen-version cont ctx label)
    (gen label)
    (cont ctx))

  (let ((version (assoc (cons cont ctx) versions)))
    (if version
        (existing (cdr version))
        (let ((label (gensym 'v)))
          (gen-version cont ctx label)
          (set! versions (cons (cons (cons cont ctx) label) versions))
          label))))

(define (gen-cont cont ctx)
  (get-cont-label cont ctx (lambda (label) (gen `(jump ,label)))))

;;-----------------------------------------------------------------------------
;; Lazy generation

(define (gen-stub thunk)
  (let ((label (gensym 'stub)))
    (set! code-stream (cons label (cons `(callback ,thunk) code-stream)))
    label))

(define (gen-expr-lazily jump expr env ctx cont)
  (let ((instr `(,jump ???)))
    (set-car! (cdr instr)
              (gen-stub (lambda ()
                          (let ((label (gensym 'lazily_generated)))
                            (set-car! (cdr instr) label)
                            (gen label)
                            (gen-expr expr env ctx cont)
                            label))))
    (gen instr)))

;;-----------------------------------------------------------------------------

(define (gen-expr expr env ctx cont)

  (match expr

    (,c when (constant? c)
     (gen `(push-lit ,c))
     (gen-cont cont (cpush ctx (if (number? c) 'num 'bool))))

    (,v when (variable? v)
     (let ((i (index-of v env)))
       (gen `(push-loc ,i))
       (gen-cont cont (cpush ctx (cref ctx i)))))

    ((println ,E)
     (gen-expr E env ctx (lambda (ctx)
                           (gen `(println))
                           (let ((ctx (cpush (cpop ctx) 'bool)))
                             (gen-cont cont ctx)))))

    ((let ,v ,E1 ,E2) when (variable? v)
     (let* ((exit (lambda (ctx)
                    (gen `(swap) `(pop))
                    (let* ((type (ctop ctx))
                           (ctx (cpush (cpop (cpop ctx)) type)))
                      (gen-cont cont ctx))))
            (body (lambda (ctx)
                    (gen-expr E2 (cons v env) ctx exit))))
       (gen-expr E1 env ctx body)))

    ;;
    ((error)
     (gen `(halt))
     (gen-cont cont ctx))

    ((fx+ ,v1 ,v2) when (and (variable? v1) (variable? v2))
     (let* ((operation
              (lambda (ctx)
                (gen '(add))
                (gen-cont cont (cpush (cpop (cpop ctx)) 'num))))
            (right
              (lambda (ctx)
                (gen-expr v2 (cons #f env) ctx operation))))
       (gen-expr v1 env ctx right)))

    ((if (number? ,v) ,E1 ,E2)
     (let* ((i (index-of v env))
            (type (cref ctx i)))
       (cond ((eq? type 'num)
              (gen-expr E1 env ctx cont))
             ((eq? type 'bool)
              (gen '(halt))
              (gen-cont cont ctx))
             (else
              (let ((test (lambda (ctx)
                            (gen-expr-lazily 'ifnum E1 env (cpop ctx) cont)
                            (gen-expr-lazily 'jump E2 env (cpop ctx) cont))))
                (gen-expr v env ctx test))))))
    ;;

    ((if ,E1 ,E2 ,E3)
     (let ((test (lambda (ctx)
                   (let ((type (cref ctx 0)))
                     (if (member type '(bool unknown))
                         ;; outcome is unclear so generate a run time test
                         (begin
                           (gen-expr-lazily 'iffalse E3 env (cpop ctx) cont)
                           (gen-expr-lazily 'jump E2 env (cpop ctx) cont))
                         ;; outcome is known to be trueish
                         (begin
                           (gen `(pop))
                           (gen-expr E2 env (cpop ctx) cont)))))))
         (gen-expr E1 env ctx test)))

    (else (error "Unknown expression"))))

(define (gen-program expr)
  (lambda (ctx)
    (gen-expr expr '(arg) ctx (lambda (ctx)
                                (gen `(halt))))))

(define (test arg-type arg-value)
  (set! code-stream (list 'main))
  (set! code-stream-end code-stream)
  (let ((prog (gen-program '(println (let a 5 (if (number? arg) (fx+ a arg) (error)))))))
    ;;
    (println "## Generate code with numeric arg")
    (let ((label (get-cont-label prog `(,arg-type) identity)))
      (println " ## Generated version is " label)
      (println " ## Stream before execution with arg == " arg-value ":")
      (print   "    ") (pp code-stream)
      (start-execution label (list arg-value))
      (println " ## Stream after execution with arg == " arg-value ":")
      (print   "    ") (pp code-stream))))

(test 'num 10)
(test 'bool #t)
(test 'unknown 10)
(test 'unknown #f)
