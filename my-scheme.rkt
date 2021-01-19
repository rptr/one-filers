#lang racket

(require racket/hash)

(define (my-and . args)
  (if (empty? args)
      #t
      (my-and-2 args)))

(define (my-and-2 args)
  (if (false? (car args))
      #f
      (if (eq? (cdr args) '())
          (car args)
          (my-and-2 (cdr args)))))

(define (my-or . args)
  (if (empty? args)
      #f
      (my-or-2 args)))

(define (my-or-2 args)
  (if (false? (car args))
      (if (empty? (cdr args))
          #f
          (my-or-2 (cdr args)))
      (car args)))

;;;;
;;;; Expression categorizers
;;;;
(define (is-value? expr)
  (or (boolean? expr)
      (number? expr)
      (null? expr)
      (string? expr)))

(define (is-variable? expr)
  (symbol? expr))

(define (is-quoted? expr)
  (eq? 'quote (car expr)))

(define (is-assign? expr)
  (eq? (car expr) 'set!))

(define (is-define? expr)
  (eq? (car expr) 'define))

(define (is-if? expr)
  (eq? (car expr) 'if))

(define (is-lambda? expr)
  (eq? (car expr) 'lambda))

(define (is-application? expr)
  (pair? expr))
;  (and (pair? expr)
;       (or (primitive-procedure? (car expr))
;           (defined-procedure? (car expr))
;           (eq? (car expr) 'and)
;           (eq? (car expr) 'or))))

(define primitive-env
  (cons (hash
   ;;
   'cons (list 'primitive cons)
   'car (list 'primitive car)
   'cdr (list 'primitive cdr)
   ;;
   '+ (list 'primitive +)
   '- (list 'primitive -)
   '* (list 'primitive *)
   '/ (list 'primitive /)
   'remainder (list 'primitive remainder)
   'modulo (list 'primitive modulo)
   'and (list 'primitive my-and)
   'or (list 'primitive my-or)
   ;;
   'eq? (list 'primitive eq?)
   'equal? (list 'primitive equal?)
   '= (list 'primitive =)
   '> (list 'primitive >)
   '< (list 'primitive <)
   '>= (list 'primitive >=)
   '<= (list 'primitive <=)
   'empty? (list 'primitive empty?)
   'number? (list 'primitive number?)
   'pair? (list 'primitive pair?)
   'symbol? (list 'primitive symbol?)
   ) 0))

(define (defined-procedure? proc env)
  (eq? (car proc) 'procedure))
;  (hash-has-key? (env-frame env) proc))

(define (primitive-procedure? proc)
  (eq? (car proc) 'primitive))
;  (hash-has-key? (env-frame primitive-env) proc))

;;;;
;;;; Expression evaluators
;;;;
(define (text-of-quote expr)
  (cadr expr))

(define (eval-assignment expr env)
  (env-set (cadr expr)
           (car (my-eval (caddr expr) env '()))
           env))

(define (eval-define expr env)
  (if (list? (cadr expr))
      (eval-define-procedure expr env)
      (env-define (cadr expr)
                  (car (my-eval (caddr expr) env '()))
                  env)))

(define (eval-define-procedure expr env)
  (env-define (caadr expr) (make-lambda (cdr (cadr expr)) (cddr expr) env) env))

(define (eval-if expr env)
  (if (car (my-eval (cadr expr) env '()))
      (my-eval (caddr expr) env '())
      (my-eval (cadddr expr) env '())))

(define (make-lambda args body env)
  (list 'procedure args body))

;;;
;;; EVAL AND APPLY
;;;
(define (my-eval expr env env-map)
  (cond
    [(is-value? expr) (cons expr env)]
    [(is-variable? expr) (cons (env-lookup-variable env expr) env)]
    [(is-quoted? expr) (cons (text-of-quote expr) env)]
    ; NOTE not sure what to return here, since there must be a return value
    [(is-assign? expr) (cons #t (eval-assignment expr env))]
    [(is-define? expr) (cons #t (eval-define expr env))]
    [(is-if? expr) (eval-if expr env)]
    [(is-lambda? expr)
     (cons (make-lambda (cadr expr) (cddr expr) env)
           env)]
    [(is-application? expr)
     (my-apply (car (my-eval (car expr) env '()))
               (eval-args (cdr expr) env)
               env)]
    [else expr]))

(define (eval-prog exprs)
  (eval-prog-2 exprs (get-global-env)))

(define (eval-prog-2 exprs env)
  (let ([res (my-eval (car exprs) env '())])
    (if (empty? (cdr exprs))
        (car res)
        (eval-prog-2 (cdr exprs) (cdr res)))))

(define (my-apply proc args env)
  (cons
   (cond
     [(primitive-procedure? proc)
      (apply-primitive-procedure proc args env)]
     [(defined-procedure? proc env)
      (apply-defined-procedure proc args env)]
     [else (error "Invalid procedure")])
   env))

(define (eval-args args env)
  (if (null? args)
      '()
      (cons (car (my-eval (car args) env '()))
            (eval-args (cdr args) env))))

(define (apply-primitive-procedure proc args env)
  (apply (cadr proc) args))

(define (apply-defined-procedure proc args env)
  (let* (
         ;[procedure (env-get env proc)]
         [params (cadr proc)]
         [body (cddr proc)]
         [new-env (env-inherit env params args)])
    (eval-prog-2 (car body) new-env)))

;;;
;;; ENVIRONMENTS
;;;

(define (list-max xs)
  (list-max-2 xs (car xs)))

(define (list-max-2 xs max)
  (if (null? xs)
      max
      (if (> (car xs) max)
          (list-max-2 (cdr xs) (car xs))
          (list-max-2 (cdr xs) max))))


(define (get-global-env)
  (cons (hash
            ; global default values
            'x 133
            ) 0))

; environment = (cons hash parent-name)
(define (get-global-env-map) '())
    
; Adds/ sets a value in an environment
(define (env-define ident val env)
      ; The identifier already exists in this env, update instead
      (cons (hash-update (car env) ident (λ _ val)
                         ; Adds a new value if value does not exist
                         (hash-set (car env) ident val)) '()))

; Update an existing value in an environment
(define (env-set ident val env)
  (cons (hash-update (car env) ident (λ x val)
                     (λ _ (error "Cannot set variable before its definition")))
                     '()))

(define (env-get env key)
  (hash-ref (env-frame env) key))

;; returns the current/ top frame
(define (env-frame env) (car env))
(define (env-parent env) (cdr env))

(define (env-inherit parent-env names values)
  (cons (make-immutable-hash (zip names values))
        parent-env))

(define (zip xs ys)
  (map cons xs ys))

(define (env-lookup-variable env name)
  (if (hash-has-key? (env-frame primitive-env) name)
      (hash-ref (env-frame primitive-env) name)
      (if (hash-has-key? (env-frame env) name)
          (hash-ref (env-frame env) name)
          (if (eq? (env-parent env) 0)
              (error "Undefined variable")
              (env-lookup-variable (env-parent env) name)))))

;;;
;;; TEST CASES
;;;

(define test-prog-1
  (list
   '(define hello (lambda (q) (* 2 (* q q))))
   '(hello 3)
   ))

(define prog-2
  (list
   '(define (bla x y) (+ x y))
   '(bla 1 2)
   ))

(define higher-order
  (list
   '(define (ya f x y) (f x y))
   '(ya + 9 5)))

(define example-prog
  (list
   '(define (even x)
      (= (remainder x 2) 0))
   '(even 10)
   '(define (length elems)
      (if (empty? elems) 0 (+ 1 (length (cdr elems)))))
   '(length (cons 1 (cons 2 (cons 3 (quote ())))))
   ))

;;; Test "real" programs
(eval-prog test-prog-1)
(eval-prog prog-2)
(eval-prog higher-order)
(eval-prog example-prog)

;;; Test expressions
(my-eval 'x (get-global-env) (get-global-env-map))
(my-eval '"hello" (get-global-env) (get-global-env-map))
(my-eval '123 (get-global-env) (get-global-env-map))
(my-eval '#t (get-global-env) (get-global-env-map))
(my-eval '(define (bla x) x) (get-global-env) '())
(my-eval '(define x 1) (get-global-env) '())
(my-eval '(lambda (x y z) 1) (get-global-env) '())
(my-eval '(set! x 100) (get-global-env) '())
(my-eval '(cons 1 (cons 2 (quote ()))) (get-global-env) '())
(my-eval '(car (cdr (cons 1 (cons 2 (quote '()))))) (get-global-env) '())
(my-eval '(if #t 123 9) (get-global-env) (get-global-env-map))
(my-eval '(+ 3 (+ (* 4 4) (* 3 3))) (get-global-env) '())
(my-eval '(- 4 3) (get-global-env) '())
(my-eval '(/ (* 4 4) (* 2 3)) (get-global-env) '())
(my-eval '(remainder 10 7) (get-global-env) '())
(my-eval '(modulo 10 7) (get-global-env) '())
(my-eval '(and 1 2) (get-global-env) '())
(my-eval '(or #f #t #f) (get-global-env) '())
(my-eval '(eq? 1 2) (get-global-env) '())
(my-eval '(equal? '() '()) (get-global-env) '())
(my-eval '(> 2 1) (get-global-env) '())
(my-eval '(< 2 1) (get-global-env) '())
(my-eval '(= 2 1) (get-global-env) '())
(my-eval '(<= 3 3) (get-global-env) '())
(my-eval '(>= 5 5) (get-global-env) '())
(my-eval '(empty? (quote ())) (get-global-env) '())
(my-eval '(number? 3) (get-global-env) '())
(my-eval '(number? #f) (get-global-env) '())
(my-eval '(pair? (cons 1 2)) (get-global-env) '())
(my-eval '(symbol? (quote hello)) (get-global-env) '())
(my-eval '(symbol? 2) (get-global-env) '())
