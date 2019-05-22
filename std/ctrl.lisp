
(defmacro progn body
  `((lambda () ,@body)))

(defmacro when (test . body)
  `(if ,test (progn ,@body)))

(defmacro return args
  `(return-from nil ,@args))

(defmacro unless (test . body)
  `(when (not ,test) ,@body))

(defmacro let (decls . body)
  (defun valid-let? (decls body)
    (if decls
        (if (cons? (car decls))
            (valid-let? (cdr decls) body))
        t))

  (assert (valid-let? decls body))

  `((lambda ,(map car decls) ,@body) ,@(map cadr decls)))

(defmacro let* (decls . body)
  (defun expand (decls)
    (if decls
        (list 'let (list (car decls))
              (expand (cdr decls)))
        (cons 'progn body)))
  (expand decls))

(defmacro cond clauses
  (defun expand (clauses)
    (if clauses
        (let* ((clause (car clauses))
               (others (cdr clauses))
               (test (car clause))
               (body (cdr clause))
               (then (cons 'progn body)))
          (if others
              (list 'if test then (expand others))
              (list 'if test then)))))
  (expand clauses))
