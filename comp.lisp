;; -*- compile-command: "./lisp load comp.lisp < comp-arith.lisp > comp-arith.cpp" -*-
(load-file "std.lisp")

(defun put-string (str)
  (stream-put-string *print-stream* str))

;; TODO useful for other code generators? -> move to a library
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

(defun make-bindings (args)
  (let ((bindings (map (lambda (x) (+ "env_def(tmp, QUOTE(" (str x) "), car(itr)); itr = cdr(itr);")) args)))
    (when bindings
      (push "Expr itr = args;" bindings))
    bindings))

(defun compile-default (stmt prefix env-name)
    (+ prefix "eval_string(" (escape (repr stmt)) ", " env-name ");"))

(defun render-expr (expr ctx env)
  (cond ((fixnum? expr)
         (println "fixnum" expr)
         (let ((r (coerce expr 'string)))
           (println expr)
           r))
        (t (error "not implemented")))
  )

(defun render-code (code ctx env)
  (let ((ret '())
        (env-name (if ctx "tmp" "env"))
        (prefix (if ctx "ret = " "")))
    (dolist (stmt code)
      (cond ((cons? stmt)
             (let* ((op (car stmt))
                    (args (cdr stmt))
                    (fun (env-lookup env op)))
               (println args)
               (let ((args (map (lambda (e) (render-expr e ctx env))
                                args)))
                 (println "after" args)
                 (if fun
                     (progn
                       (push (+ "// " (repr stmt)) ret)
                       (println "test")
                       (println fun)
                       (push (+ prefix
                                (let ((fname (defun-mangled-name fun)))
                                  (if (c-operator? fname)
                                      (+ (join (+ " " fname " ") args) " //operator")
                                    (+ fname "(nil, " env-name "); // TODO add args"))))
                             ret))

                   (push (compile-default stmt prefix env-name) ret))))
             )

            ;;((cons? stmt)
            ;; (let ((op (car stmt)))
            ;;   (let ((itr (env-find env op)))
            ;;     (if itr
            ;;         (push (+ (mangle (car itr)) "(nil, nil);") ret)
            ;;         (push (compile-default stmt env-name) ret)))))

            ((println? stmt env) ; TODO generalize this
             (push (+ "// " (repr stmt)) ret)
             (push (+ prefix "b_println(read_from_string(" (escape (repr (map std:eval (cdr stmt)))) "), env, NULL);") ret)) ; TODO need to drop the args

            (t
             (push (compile-default stmt prefix env-name) ret))))

    (nreverse ret)))

(defun make-defun (name mangled-name)
  (list 'fun name mangled-name))
(defun make-defun-mangled (name c-fix-position) (make-defun name (mangle name)))
(defun defun-name (f) (caar f))
(defun defun-mangled-name (f) (caddr f))

(defun compile-defun (env name params body)
  (let ((fun (make-defun-mangled name)))
    (env-bind env name fun)
    (let ((bindings (make-bindings params)))
      (emit-tree `(,(+ "Expr " (defun-mangled-name fun) "(Expr args, Expr env)")
                   "{"
                   ("Expr ret = nil;"
                    "Expr tmp = make_env(env);"
                    ,@bindings
                    ,@(render-code body name env)
                    "return ret;")
                   "}"))))  )

(let ((c-operator-names '("+")))
  (defun-global c-operator? (fname)
    (println c-operator-names fname)
    (index c-operator-names fname)))

(defun blah (code env)
  (println (make-defun '+ "+"))
  ;FIXME
  (env-bind env '+ (make-defun '+ "+" 1))

  ;; TODO add a flag to disable this
  (emit-tree '(""
               "#define LISP_IMPLEMENTATION"
               "#include \"runtime.hpp\""))

  

  (dolist (stmt code)
    (cond ((defun? stmt)
           (put-string "\n// ")
           (emit-tree (render-stmt stmt nil))
           (let (((_ name args . body) stmt))
             (compile-defun env name args body)))
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

