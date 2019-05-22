
(load-file "std.lisp")

;;; error

(defun ERROR args
  (funcall println args)
  (throw args)
  '#error)

;;; closure

(defun MAKE-CLOSURE (tag env params body)
  (cons (cons tag env) (cons params body)))

(defun CLOSURE-TAG (exp)
  (caar exp))

(defun CLOSURE-ENV (exp)
  (cdar exp))

(defun CLOSURE-PARAMS (exp)
  (cadr exp))

(defun CLOSURE-BODY (exp)
  (cddr exp))

(defun TAGGED? (exp tag)
  (and (cons? exp)
       (cons? (car exp))
       (eq (CLOSURE-TAG exp) tag)))

;;; function

(defparameter +FUNCTION-TAG+ (gensym))

(defun MAKE-FUNCTION (env params body)
  (MAKE-CLOSURE +FUNCTION-TAG+ env params body))

(defun FUNCTION? (exp)
  (TAGGED? exp +FUNCTION-TAG+))

;;; macro

(defparameter +MACRO-TAG+ (gensym))

(defun MAKE-MACRO (env params body)
  (MAKE-CLOSURE +MACRO-TAG+ env params body))

(defun MACRO? (exp)
  (TAGGED? exp +MACRO-TAG+))

;;; env

(defun MAKE-ENV (outer)
  (cons (cons nil nil) outer))

(defun ENV-OUTER (env)
  (cdr env))

(defun ENV-BIND (env var val)
  (let ((vals (ENV-FIND env var)))
    (if vals
        (rplaca vals val)
        (ENV-PUSH env var val))))

(defun ENV-DESTRUCTURING-BIND (env var val)
  (cond ((nil? var)) ; do nothing

        ((cons? var)
         (ENV-DESTRUCTURING-BIND env (car var) (car val))
         (ENV-DESTRUCTURING-BIND env (cdr var) (cdr val)))

        (t
         (ENV-BIND env var val))))

(defun ENV-LOOKUP (env var)
  (let ((iter (ENV-FIND-GLOBAL env var)))
    (if iter
        (car iter)
        (ERROR  "unbound variable" var))))

(defun ENV-PUSH (env var val)
  (let ((pair (car env)))
    (rplaca pair (cons var (car pair)))
    (rplacd pair (cons val (cdr pair)))))

(defun ENV-FIND (env var)
  (defun helper (vars vals)
    (cond ((nil? vars)
           nil)

          ((eq (car vars) var)
           vals)

          (t
           (helper (cdr vars) (cdr vals)))))

  (let (((vars . vals) (car env)))
    (helper vars vals)))

(defun ENV-FIND-GLOBAL (env var)
  (if env
      (let ((vals (ENV-FIND env var)))
        (if vals
            vals
            (ENV-FIND-GLOBAL (ENV-OUTER env) var)))
      nil))

;;; eval

(defun MAKE-CALL-ENV (name vals env)
  (let* ((fenv (CLOSURE-ENV name))
         (denv fenv) ; TODO add *special*
         (cenv (MAKE-ENV denv))
         (vars (CLOSURE-PARAMS name)))
    (ENV-DESTRUCTURING-BIND cenv vars vals)
    cenv))

(defun EVAL (exp env)
  (cond
    ((SELF-EVAL? exp)
     exp)

    ((VARIABLE? exp)
     (ENV-LOOKUP env exp))

    ((cons? exp)
     (let ((name (car exp))
           (args (cdr exp)))

       (cond
         ((FUNCTION? name)
          (EVAL-BODY (CLOSURE-BODY name)
                     (MAKE-CALL-ENV name (EVAL-LIST args env) env)))

         ((MACRO? name)
          (EVAL (EVAL-BODY (CLOSURE-BODY name)
                           (MAKE-CALL-ENV name args env))
                env))

         ((function? name) ;; fexpr
          (name args env))
          ;;(name (EVAL-LIST args env) env))

         ((eq name 'quote)
          (car args))

         ((eq name 'if)
          (let (((_ test then else) exp))
            (if (EVAL test env)
                (EVAL then env)
                (EVAL else env))))

         ((eq name 'eq)
          (eq (EVAL (cadr  exp) env)
              (EVAL (caddr exp) env)))

         ((eq name 'cons)
          (cons (EVAL (cadr  exp) env)
                (EVAL (caddr exp) env)))

         ((eq name 'car)
          (car (EVAL (cadr  exp) env)))

         ((eq name 'cdr)
          (cdr (EVAL (cadr  exp) env)))

         ((eq name 'lambda)
          (let ((params (cadr exp))
                (body   (cddr exp)))
            (MAKE-FUNCTION env params body)))

         ((eq name 'syntax)
          (let ((params (cadr exp))
                (body   (cddr exp)))
            (MAKE-MACRO env params body)))

         ;;; core stuff, should go into builtins

         ;; TODO add builtins
         ((eq name 'println)
          (funcall println (EVAL-LIST args env)))

         (t
          (let ((temp (EVAL name env)))
            (if (eq temp name)
                (ERROR"recursive operator" temp "in" exp)
                (EVAL (cons temp args) env)))))))

    (t
     (ERROR"cannot evaluate" exp))))

(defun EVAL-BODY (body env)
  (let ((ret nil))
    (dolist (stmt body)
      (= ret (EVAL stmt env)))
    ret))

(defun EVAL-LIST (exps env)
  (let ((ret nil))
    (dolist (exp exps)
      (push (EVAL exp env) ret))
    (nreverse ret)))

(defun SELF-EVAL? (exp)
  (or (nil? exp)
      (string? exp)
      (fixnum? exp)))

(defun VARIABLE? (exp)
  (or (symbol? exp)
      (gensym? exp)))

(defun fun->fexpr (fun) ; TODO use this
  (lambda (args env)
    (fun (EVAL-LIST args) env)))

(defun fun->fexpr (fun)
  fun)

(defun MAKE-CORE-ENV ()
  (let ((env (MAKE-ENV nil)))
    (ENV-BIND env 't 't)
    (ENV-BIND env 'load-file (fun->fexpr (lambda (args env)
                                           (LOAD-FILE (car args) env))))

    (ENV-BIND env 'env-bind
              (lambda (args env)))

    (ENV-BIND env 'defmacro
              (lambda ((name args . body) env)
                (ENV-BIND env name (MAKE-MACRO env args body))))

    (ENV-BIND env 'defun
              (lambda ((name args . body) env)
                (ENV-BIND env name (MAKE-FUNCTION env args body))))

    (ENV-BIND env 'defparameter
              (lambda (args env)
                (ENV-BIND env (car args) (and (cdr args)
                                              (EVAL (cadr rest) env)))))

    ;;(println env)
    env))

(defun EVAL-CORE (exp)
  (let ((env ((MAKE-CORE-ENV))))
    (EVAL exp env)))

(defun LOAD-FILE (ifn env)
  ;;(println 'LOADING... ifn)
  (dolist (exp (read-file ifn))
    ;;(println 'EVAL exp)
    (EVAL exp env)))

(dolist (ifn *argv*)
  (let ((env (MAKE-CORE-ENV)))
    (LOAD-FILE ifn env)))
