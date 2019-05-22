
(load-file "lisp2x.lisp")

(def SHOW_COMMENTS t)

(defun render-file (exps env)
  (let ((ret '())
        (1st t))

    ;;(println 'env env)
    (if exps (push "" ret))

    (dolist (exp exps)
      (if 1st
          (= 1st nil)
          (unless (equal "" (car ret))
            (push "" ret))) ;; TODO filter defmacro calls before calling this (multi-pass architecture)
      (when SHOW_COMMENTS
        (push (+ "# " (repr exp)) ret))

      (dolist (line (render-stmt exp env nil))
        (push line ret)))

    (nreverse ret)))

(defun render-stmt (exp env ctx)
  (cond
    ((defun? exp)
     (render-defun exp env ctx))

    ((named-op? exp 'import)
     (render-import exp env ctx))

    ((named-op? exp 'dolist)
     (render-dolist exp env ctx))

    ((if? exp)
     (render-if exp env ctx))

    ((named-op? exp 'when)
     (render-when exp env ctx))

    ((return? exp)
     (render-return exp env ctx))

    ((named-op? exp 'defmacro)
     (let ((name (cadr exp))
           (args (caddr exp))
           (body (cdddr exp)))
       ;;(println name args body)
       ;;(println (make-macro env args body))
       (env-def env name (make-macro env args body))
       nil))

    ((and (cons? exp) (env-has env (car exp)))
     ;;(println ' env (env-has env (car exp)))
     (let ((mac (env-lookup env (car exp))))
       ;;(println mac)
       ;; TODO that calling apply like this is huge a security risk
       ;; TODO macroexpand-1 does not take env as an arg
       (render-stmt (apply mac (cdr exp)) env exp)))

    (t
     (list (render-expr exp env ctx)))))

(defun render-body (exp env ctx)
  (if exp
      (let ((ret '()))
        (dolist (stmt exp)
          (= ret (append ret (render-stmt stmt env exp)))) ;; TODO this is hopelessly slow
        ret)
      '("pass")))

(defun render-args (exp env ctx)
  (join ", " (map (lambda (arg) (render-expr arg env ctx)) exp)))

(defun render-defun (exp env ctx)
  (let ((name (cadr exp))
        (args (caddr exp))
        (body (cdddr exp)))
    (let ((head (+ "def " (render-name name) "(" (render-args args env exp) "):"))
          (code (render-body body env exp)))
      (list head code))))

(defun render-import (exp env ctx)
  (let ((args (cdr exp))
        (ret '()))
    (dolist (name args)
      (push (+ "import " (render-expr name env exp)) ret))
    (nreverse ret)))

(defun render-import (exp env ctx)
  (list (+ "import "
           (join ", " (map (lambda (name)
                             (render-expr name env exp))
                           (cdr exp))))))

(defun render-dolist (exp env ctx)
  (defun EXPR (x)
    (render-expr x env exp))

  (defun BODY (x)
    (render-body x env exp))

  (let (((_ (var val) . body) exp))
    (list (+ "for " (EXPR var) " in " (EXPR val) ":")
          (BODY body))))

(defun render-if (exp env ctx)
  (let ((test (cadr exp))
        (then (caddr exp))
        (rest (cdddr exp))
        (ret '()))

    (push (+ "if " (render-expr test env exp) ":") ret) ;; TODO use reverse currying?
    (push (render-stmt then env exp) ret)
    (when rest
      (push "else:" ret)
      (push (render-stmt (car rest) env exp) ret))

    (nreverse ret)))

(defun render-when (exp env ctx)
  (let ((test (cadr exp))
        (body (cddr exp))
        (ret '()))

    (push (+ "if " (render-expr test env exp) ":") ret)
    (push (render-body body env exp) ret)
    (nreverse ret)))

(defun render-return (exp env ctx)
  (let ((args (cdr exp)))
    (if args
        (list (+ "return " (render-expr (car args) env exp)))
        (list "return"))))

(defun constant? (exp)
  (or (string? exp)
      (number? exp)))

(defun constant-list? (exp)
  (cond ((nil? exp) t)

        ((cons? exp)
         (let ((head (car exp))
               (tail (cdr exp)))
           (and (or (constant? head)
                    (constant-list? head))
                (constant-list? tail))))

        (t nil)))

(defun render-constant-list (exp)
  (defun render-list (exp)
    (+ "[" (join ", " (map render-item exp)) "]"))

  (defun render-item (exp)
    (cond ((nil? exp)  (render-list exp))
          ((cons? exp) (render-list exp))
          ((string? exp) (repr exp))
          ((number? exp) exp)
          (t (error "not a constant list"))))

  (render-list exp))

(defun render-expr (exp env ctx)
  (cond ((nil? exp)
         "None")

        ((symbol? exp)
         (render-name exp))

        ((fixnum? exp)
         (repr exp))

        ((string? exp)
         (repr exp))

        ((cons? exp)
         (cond ((any-named-op? exp '< '+ '- '== '=)
                (render-binop exp env ctx))

               ((named-op? exp 'not)
                (+ "not " (render-expr (cadr exp) env exp)))

               ((named-op? exp 'quote)
                (let ((arg (cadr exp)))
                  ;;(println '# arg)
                  (render-constant-list arg)))

               (t
                (render-call exp env ctx))))
        (t
         (error "cannot compile expr " exp))))

(defun render-binop (exp env ctx)
  (let* ((name (car exp))
         (args (cdr exp))
         (sep  (symbol-name name)))
    (unless (defun? ctx)
      (= sep (+ " " sep " ")))
    (join sep (map (lambda (x) (render-expr x env exp)) args))))

(defun render-call (exp env ctx)
  (let ((op   (car exp))
        (args (cdr exp))
        (line "")
        (1st t))

    (+= line (render-expr op env exp))
    (+= line "(")
    (dolist (arg args)
      (if 1st
          (= 1st nil)
          (+= line ", "))
      (+= line (render-expr arg env exp)))
    (+= line ")")
    line))

(defun render-name (exp) ;; TODO need string:endswith etc.
  (when (eq exp 'disgusted?)
    (= exp 'is_disgusted))
  (symbol-name exp))

(dolist (arg *argv*)
  (cond ((equal arg "--no-comments")
         (= SHOW_COMMENTS nil)) ; TODO this does not work with specials!!!
        (t
         (emit-tree (render-file (read-file arg) (make-env nil))))))
