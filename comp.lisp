
(load-file "std.lisp")

(defun put-string (str)
  (stream-put-string *print-stream* str))

(defun line-tree-to-stream (tree out indent)
  (defun emit (str)
    (stream-put-string out str))

  (defun helper (tree prefix)
    (dolist (item tree)
      (cond ((string? item)
             (emit prefix)
             (emit item)
             (emit "\n"))

            (t
             (helper item (+ indent prefix))))))

  (helper tree ""))

(defun emit-tree (tree)
  (line-tree-to-stream tree *print-stream* "    "))

(defun render-expr (exp ctx)
  (list (repr exp)))

(defun render-stmt (exp ctx)
  (cond (t
         (render-expr exp ctx))))

(defun op? (exp name)
  (and (cons? exp)
       (eq name (car exp))))

(defun defun? (exp)
  (op? exp 'defun))

(defun println? (exp env)
  (op? exp 'println))

(defun mangle (name)
  ;;(repr name))
  (+ "_fun" (str (meta:expr-data name)))) ; TODO use hash?

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

(defun blah (code env)
  (defun comp-default (stmt prefix env-name)
    (+ prefix "eval_string(" (escape (repr stmt)) ", " env-name ");"))

  (defun render-code (code ctx env)
    (let ((ret '())
          (env-name (if ctx "tmp" "env"))
          (prefix (if ctx "ret = " "")))
      (dolist (stmt code)
        (cond ((cons? stmt)
               (let* ((op (car stmt))
                      (args (cdr stmt))
                      (it (env-find env op)))
                 (if (and nil it)
                     (progn
                       (push (+ "// " (repr stmt)) ret)
                       (push (+ prefix (mangle op) "(nil, " env-name "); // TODO add args") ret))

                     (push (comp-default stmt prefix env-name) ret))))

              ;;((cons? stmt)
              ;; (let ((op (car stmt)))
              ;;   (let ((itr (env-find env op)))
              ;;     (if itr
              ;;         (push (+ (mangle (car itr)) "(nil, nil);") ret)
              ;;         (push (comp-default stmt env-name) ret)))))

              ((println? stmt env) ; TODO generalize this
               (push (+ "// " (repr stmt)) ret)
               (push (+ prefix "b_println(read_from_string(" (escape (repr (map std:eval (cdr stmt)))) "), env, NULL);") ret)) ; TODO need to drop the args

              (t
               (push (comp-default stmt prefix env-name) ret))))

      (nreverse ret)))

  ;; TODO add a flag to disable this
  (emit-tree '(""
               "#define LISP_IMPLEMENTATION"
               "#include \"runtime.hpp\""))

  (defun make-bindings (args)
    (let ((ret nil))
      (when args
        (= ret (map (lambda (x) (+ "env_def(tmp, QUOTE(" (str x) "), car(itr)); itr = cdr(itr);")) args))
        (push "Expr itr = args;" ret))
      ret))

  (dolist (stmt code)
    (cond ((defun? stmt)
           (stream-put-string *print-stream* "\n// ")
           (emit-tree (render-stmt stmt nil))
           (let (((_ name args . body) stmt))
             (env-bind env name name)
             (let ((bindings (make-bindings args)))
               (emit-tree `(,(+ "Expr " (mangle name) "(Expr args, Expr env)")
                             "{"
                             ("Expr ret = nil;"
                              "Expr tmp = make_env(env);"
                              ,@bindings
                              ,@(render-code body stmt env)
                              "return ret;")
                             "}")))))
          (t)))

  ;; TODO don't emit if you find main?
  (emit-tree `(""
               "int main(int argc, char ** argv)" "{"
               ("try"
                "{"
                ("system_init();"
                 "Expr env = make_env(nil);"
                 "system_bind_core(env);"
                 ,@(render-code code nil env)
                 ;;"_fun269(LIST1(make_string(\"foo\")), env);"
                 "system_quit();"
                 "return 0;")
                "}"
                "catch (Expr err)"
                "{"
                ("fprintf(stderr, \"error: %s\\n\", repr(err));"
                 "return 1;")
                "}")
               "}")))

(let ((code (read-all-from-stream *read-stream*))
      (env (make-env *env*)))

  (blah code env))

