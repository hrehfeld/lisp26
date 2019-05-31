;; -*- compile-command: "./lisp load lisp2cpp.lisp tests/programs/comp-arith.lisp > comp-arith.lisp" -*-

(load-file "lisp2x.lisp")

(defconstant +show-comment+ nil)

(defconstant $DEFMAIN '$defmain)

(defun split-file (exps)
  (binary-classify defun? exps))

(defun compile-expr (exp env)
  (let ((r '())))
  (cond
   ((symbol? exp)
    ;;TODO: check if exists
    (render-name exp))

    ((fixnum? exp)
     (str exp))

    ((cons? exp)
     (cond
       ((coerce? exp)
        (render-coerce exp env ctx))

       ((if? exp)
        (+ (render-expr (cadr exp) env exp)
           " ? "
           (render-expr (caddr exp) env exp)
           " : "
           (render-expr (cadddr exp) env exp)))

       ((any-named-op? exp
                       '+= '^= '>> '<<
                       '+ '- '* '/ '< '>)
        (let* ((use-parens (< (prec exp) (prec ctx)))
               (pl (if use-parens "(" ""))
               (pr (if use-parens ")" "")))
          (+ pl (join
                 (+ " " (render-expr (car exp) env exp) " ")
                 (map (lambda (x) (render-expr x env exp)) (cdr exp)))
             pr)))
       (t
        (render-call exp env ctx))))

    (t
     (error "cannot compile expr " exp)))
  )

(defun compile-assign-env (exp env)
  (cond ((any-named-op? exp 'let* 'let)
         (let (((let-sym let-decls . let-body) exp))
           (list let-sym (make-env env) let-decls (map (curry-2nd compile-assign-env env) let-body))))
        (t exp)))

;; (defun compile-assign-envs (exprs env)
;;   (map compile-assign-env exprs))

(defun compile-file (exps env)
  (let (((exps . main-body) (split-file exps)))
    (println exps)
    (println main-body)
    (println (map (curry-2nd compile-assign-env env) main-body))
    `(,$DEFMAIN ,@main-body)
))


(defun render-file (exps env)
  (let ((ret '())
          (1st t))

      ;;(println 'env env)
      ;; emit empty line
      (if exps (push "" ret))

      (dolist (exp exps)
        (if 1st
            (= 1st nil)
          (unless (equal "" (car ret))
            (push "" ret))) ;; TODO filter defmacro calls before calling this (multi-pass architecture)
        (when +show-comment+
          (push (+ "// " (repr exp)) ret))
        (= ret (append (nreverse (render-stmt exp env nil)) ret)))

      (nreverse ret)))

;;; stmt

(defun render-stmt (exp env ctx)
  ;;(println '// 'STMT)
  (cond
    ((defun? exp)
     (render-defun exp env ctx))

    ((return? exp)
     (render-return exp env ctx))

    ((when? exp)
     (render-when exp env ctx))

    ((named-op? exp 'let*)
     (render-let* exp env ctx))

    (t
     (list (+ (render-expr exp env ctx) ";")))))

(defun render-body (exp env ctx)
  (let ((ret '()))
    (dolist (stmt exp)
      (= ret (append ret (render-stmt stmt env exp)))) ;; TODO this is hopelessly slow
    ret))

(defun declare-type-parse (type-map (type . names))
  (dolist (name names)
    (env-bind type-map name type)))

(defun strip-declarations (body type-map)
  (when body
    (if (declare? (car body))
        ;; TODO: support more declares
        (let (((_decl-sym . decls) (car body)))
          (dolist ((decl-kind . decl-args) decls)
            (cond ((== decl-kind 'type)
                   (declare-type-parse type-map decl-args))
                  (t (error "unsupported declare kind"))))
          (strip-declarations (cdr body) type-map))
      body)))

(defun render-defun (exp env ctx)
  ;;(println '// 'DEFUN)
  (let (((_ name args . body) exp)
        (type-map (make-env nil))
        (temp nil))

    (env-bind type-map name 'Expr)
    (dolist (arg args)
      (env-bind type-map arg 'Expr))

    (= body (strip-declarations body type-map))

    ;;(println type-map)

    (let* ((return-type (env-lookup type-map name))
           (head (+ (render-type return-type env exp) " " (render-name name) "(" (join ", " (map (lambda (arg) (render-type (env-lookup type-map arg) env exp)) args)) ")"))
          (code (render-body body env exp)))
      (list head "{" code "}"))))

(defun render-return (exp env ctx)
  (if (cdr exp)
      (list (+ "return " (render-expr (cadr exp) env exp) ";"))
      (list "return;")))

(defun render-when (exp env ctx)
  (list (+ "if (" (render-expr (cadr exp) env exp) ")")
        "{"
        (render-body (cddr exp) env exp)
        "}"))

(defun render-let* (exp env ctx)
  (let (((_ decls . body) exp))
    (let ((code (render-body body env exp)))
      (dolist (decl (reverse decls))
        (let (((var val) decl))
          ;;(println var val)
          (push (+ "Expr "
                   (render-name var)
                   " = "
                   (render-expr val env exp)
                   ";")
                code)))
      (if (defun? ctx)
          code
          (list "{" code "}")))))

;;; expr

(defun prec (exp)
  (cond
    ((any-named-op? exp '+ '-)
     0)
    ((any-named-op? exp '* '/)
     1)
    (t
     -1)))

(defun render-expr (exp env ctx)
  (println '// 'EXPR exp)
  (cond
    ((symbol? exp)
     (render-name exp))

    ((fixnum? exp)
     (str exp))

    ((cons? exp)
     (cond
       ((coerce? exp)
        (render-coerce exp env ctx))

       ((if? exp)
        (+ (render-expr (cadr exp) env exp)
           " ? "
           (render-expr (caddr exp) env exp)
           " : "
           (render-expr (cadddr exp) env exp)))

       ((any-named-op? exp
                       '+= '^= '>> '<<
                       '+ '- '* '/ '< '>)
        (let* ((use-parens (< (prec exp) (prec ctx)))
               (pl (if use-parens "(" ""))
               (pr (if use-parens ")" "")))
          (+ pl (join
                 (+ " " (render-expr (car exp) env exp) " ")
                 (map (lambda (x) (render-expr x env exp)) (cdr exp)))
             pr)))
       (t
        (render-call exp env ctx))))

    (t
     (error "cannot compile expr " exp))))

(defun render-coerce (exp env ctx)
  (+ "("
     (render-type (caddr exp) env exp)
     ") "
     (render-expr (cadr exp) env exp)))

(defun render-call (exp env ctx)
  ;;(println '// 'CALL)
  (let ((name (car exp))
        (args (cdr exp))
        (line "")
        (1st t))

    (+= line (render-expr name env exp))
    (+= line "(")
    (dolist (arg args)
      (if 1st
          (= 1st nil)
          (+= line ", "))
      (+= line (render-expr arg env exp)))
    (+= line ")")
    line))

(defun render-type (exp env ctx)
  (cond ((cons? exp)
         (join " " (map render-name exp)))
        (t
         (render-name exp))))

(defun render-name (exp)
  (symbol-name exp))

(dolist (arg *argv*)
  (let ((exps (read-file arg)))
    (let ((exps (compile-file exps (make-env))))
      (println exps)
      ;(emit-tree (render-file  (make-env nil)))
      )))
