
(load-once "std.lisp")

(defun env-push (env var val)
  (let ((pair (car env)))
    (rplaca pair (cons var (car pair)))
    (rplacd pair (cons val (cdr pair)))))

(defun env-find (env var)
  (defun helper (vars vals)
    (cond ((nil? vars)
           nil)

          ((eq (car vars) var)
           vals)

          (t
           (helper (cdr vars) (cdr vals)))))

  (let ((pair (car env)))
    (let ((vars (car pair))
          (vals (cdr pair)))
      (helper vars vals))))

(defun env-find-global (env var)
  ;;(println 'env-find-global env var)
  (if env
      (let ((vals (env-find env var)))
        (if vals
            vals
            (env-find-global (env-outer env) var)))
      nil))

(defun make-env (outer)
  (cons (cons nil nil) outer))

(defun env-outer (env)
  (cdr env))

(defun env-bind (env var val)
  (let ((vals (env-find env var)))
    (if vals
        (rplaca vals val)
        (env-push env var))))

(defun env-destructuring-bind (env var val)
  (cond ((nil? var)) ; do nothing

        ((cons? var)
         (env-destructuring-bind env (car var) (car val))
         (env-destructuring-bind env (cdr var) (cdr val)))

        (t
         (env-bind env var val))))

(defun env-lookup (env var)
  (let ((iter (env-find-global env var)))
    (if iter
        (car iter)
        (throw 'eval (list '#:error  "unbound variable" var)))))
