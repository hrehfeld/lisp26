
(load-once "std.lisp")
(load-once "env.lisp")
(load-once "closure.lisp")
(load-once "builtin.lisp")

(defconstant _RED     "\x1b[31m")
(defconstant _GREEN   "\x1b[32m")
(defconstant _YELLOW  "\x1b[33m")
(defconstant _BLUE    "\x1b[34m")
(defconstant _MAGENTA "\x1b[35m")
(defconstant _CYAN    "\x1b[36m")
(defconstant _RESET   "\x1b[0m")

(defconstant +true+  't)
(defconstant +false+ 'nil)

(defun my/print args ; HACK we cannot do this with println
  (dolist (arg args)
    (if (string? arg)
        (stream-put-string *print-stream* arg)
        (stream-put-string *print-stream* (str arg)))))

(defun self-eval? (exp)
  (or (nil? exp)
      (eq +true+ exp)
      (eq +false+ exp)
      (fixnum? exp)
      (string? exp)))

(defun variable? (exp)
  (or (symbol? exp)
      (gensym? exp)))

(defun host->bool (exp)
  (if exp
      +true+
      +false+))

(defun bool->host (exp)
  (cond ((eq +true+ exp)
         t)
        ((eq +false+ exp)
         nil)
        (t
         (error "not a bool"))))

(defun make-call-env (op vals env)
  (let* ((fenv (closure-env op))
         (denv fenv) ; TODO *special*, see eval.cppx
         (cenv (make-env denv))
         (vars (closure-params op)))

    (env-destructuring-bind cenv vars vals)

    cenv))

(defun apply (op vals env)
  (cond ((closure? op)
         (eval-body (closure-body op) (make-call-env op vals env)))

        ((builtin? op) ; TODO do we need to apply or funcall?
         ((builtin-fun op) vals env))

        (t
         (throw 'eval (list '#:error "cannot apply" op)))))

(defun eval-list (exps env)
  (let ((ret nil))
    (dolist (exp exps)
      (push (eval exp env) ret))
    (nreverse ret)))

(defun eval-body (body env)
  (let ((ret nil))
    (dolist (stmt body)
      (= ret (eval stmt env)))
    ret))

(defun named-op? (exp name)
  (and (cons? exp)
       (eq name (car exp))))

(defun backquote? (exp)
  (named-op? exp 'backquote))

(defun eval-backquote (exp env)
  ;;(defun unquote? (exp)
  ;;  (named-op? exp 'unquote))
  ;;
  ;;(defun unquote-splicing? (exp)
  ;;  (named-op? exp 'unquote-splicing))
  ;;
  ;;(defun bq (exp env)
  ;;  (println 'BQ exp)
  ;;  (if (cons? exp)
  ;;      (if (unquote? exp)
  ;;          (eval (cadr exp) env)
  ;;          (bq-list exp env))
  ;;      exp))
  ;;
  ;;(defun bq-list (seq env)
  ;;  (if seq
  ;;      (let ((item (car seq))
  ;;            (rest (cdr seq)))
  ;;        (if (unquote-splicing? item)
  ;;            (append (eval (cadr item) env) (bq-list rest env))
  ;;            (cons (bq item env) (bq-list rest env))))))
  ;;
  ;;(bq (cadr exp) env)

  'BACKQUOTE)

(defun catch? (exp)
  (named-op? exp 'catch))

(defun eval-catch (exp env)
  'CATCH) ; TODO

(defun throw? (exp)
  (named-op? exp 'throw))

(defun eval-throw (exp env)
  (let ((tag    (eval (cadr  exp) env))
        (result (eval (caddr exp) env)))
    (throw (list 'throw tag result))))

(defun block? (exp)
  (named-op? exp 'block))

(defun eval-block (exp env)
  'BLOCK) ; TODO

(defun return-from? (exp)
  (named-op? exp 'return-from))

(defun eval-return-from (exp env)
  (let ((name (cadr exp)))
    (if (cddr exp)
        (throw (list 'return-from name (eval (caddr exp env))))
        (throw (list 'return-from name)))
    nil))

(defun eval (exp env)
  ;;(println 'my/eval exp env)
  (cond ((self-eval? exp)
         exp)

        ;; TODO do we want to allow overriding this?
        ((eq exp '*env*)
         env)

        ((variable? exp)
         (env-lookup env exp))

        ((cons? exp)
         (let ((op (car exp)))
           (cond ((or (function? op) (builtin? op))
                  (apply op (eval-list (cdr exp) env) env))

                 ((macro? op)
                  (eval (apply op (cdr exp) env) env))

                 ((eq op 'quote)
                  (cadr exp))

                 ((eq op 'if)
                  (if (bool->host (eval (cadr exp) env))
                      (eval (caddr exp) env)
                      (eval (cadddr exp) env)))

                 ((eq op 'eq)
                  (host->bool (eq (eval (cadr exp) env)
                                  (eval (caddr exp) env))))

                 ((eq op 'cons)
                  (cons (eval (cadr exp) env)
                        (eval (caddr exp) env)))

                 ((eq op 'car)
                  (car (eval (cadr exp) env)))

                 ((eq op 'cdr)
                  (cdr (eval (cadr exp) env)))

                 ((eq op 'lambda)
                  (let ((params (cadr exp))
                        (body   (cddr exp)))
                    (make-function env params body)))

                 ((eq op 'syntax)
                  (let ((params (cadr exp))
                        (body   (cddr exp)))
                    (make-macro env params body)))

                 ((backquote? exp)
                  (eval-backquote exp env))

                 ((catch? exp)
                  (eval-catch exp env))

                 ((throw? exp)
                  (eval-throw exp env))

                 ((block? exp)
                  (eval-block exp env))

                 ((return-from? exp)
                  (eval-return-from exp env))

                 ;; TODO builtins

                 ((named-op? exp 'make-hash)
                  (std:make-hash))

                 ((named-op? exp 'hash-put)
                  (std:hash-put (eval (cadr exp) env)
                                (eval (caddr exp) env)
                                (eval (cadddr exp) env)))

                 ((named-op? exp 'hash-get)
                  (std:hash-get (eval (cadr exp) env)
                                (eval (caddr exp) env)))

                 ((named-op? exp 'println)
                  ;;(apply println (cdr exp))
                  (println (cadr exp)))

                 (t
                  ;;(list '#:error "cannot evaluate op " op)

                  (let ((new-op (eval op env)))
                    (eval (cons new-op (cdr exp)) env))))))

        (t
         (throw 'eval (list '#:error "cannot evaluate " exp)))))

(let ((env (make-env nil))
      (num-tests 0)
      (num-failed 0)
      (tests (read-file "eval.test")))

  ;; HACK
  (env-def env '#t t)
  (env-def env '#f nil)
  ;;(env-def env '... '...)

  (dolist (test tests) ; TODO add implicit destructuring bind
    ;;(catch 'eval
    (progn
      (let ((src (car test))
            (ref (caddr test)))

        (println src)
        (let ((val (eval src env)))
          (+= num-tests 1)
          (cond ((equal val ref)
                 (my/print _GREEN "PASS " _RESET src " => " ref "\n"))

                (t
                 (+= num-failed 1)
                 (my/print _RED   "FAIL " _RESET src " => " val " != " ref "\n")))))))

  (if (> num-failed 0)
      (my/print _RED   "FAIL " _RESET num-failed '/ num-tests " test(s)\n")
      (my/print _GREEN "PASS " _RESET num-tests '/ num-tests " test(s)\n")))
