
#define LISP_IMPLEMENTATION
#include "runtime.hpp"

// (defun put-string (str) (stream-put-string *print-stream* str))
Expr _fun306(Expr args, Expr env)
{
    Expr ret = nil;
    Expr tmp = make_env(env);
    Expr itr = args;
    env_def(tmp, QUOTE(str), car(itr)); itr = cdr(itr);
    ret = eval_string("(stream-put-string *print-stream* str)", tmp);
    return ret;
}

// (defun line-tree-to-stream (tree out indent) (defun emit (str) (stream-put-string out str)) (defun helper (tree prefix) (dolist (item tree) (cond ((string? item) (emit prefix) (emit item) (emit "\n")) (t (helper item (+ indent prefix)))))) (helper tree ""))
Expr _fun307(Expr args, Expr env)
{
    Expr ret = nil;
    Expr tmp = make_env(env);
    Expr itr = args;
    env_def(tmp, QUOTE(tree), car(itr)); itr = cdr(itr);
    env_def(tmp, QUOTE(out), car(itr)); itr = cdr(itr);
    env_def(tmp, QUOTE(indent), car(itr)); itr = cdr(itr);
    ret = eval_string("(defun emit (str) (stream-put-string out str))", tmp);
    ret = eval_string("(defun helper (tree prefix) (dolist (item tree) (cond ((string? item) (emit prefix) (emit item) (emit \"\\n\")) (t (helper item (+ indent prefix))))))", tmp);
    ret = eval_string("(helper tree \"\")", tmp);
    return ret;
}

// (defun emit-tree (tree) (line-tree-to-stream tree *print-stream* "    "))
Expr _fun312(Expr args, Expr env)
{
    Expr ret = nil;
    Expr tmp = make_env(env);
    Expr itr = args;
    env_def(tmp, QUOTE(tree), car(itr)); itr = cdr(itr);
    ret = eval_string("(line-tree-to-stream tree *print-stream* \"    \")", tmp);
    return ret;
}

// (defun render-expr (exp ctx) (list (repr exp)))
Expr _fun313(Expr args, Expr env)
{
    Expr ret = nil;
    Expr tmp = make_env(env);
    Expr itr = args;
    env_def(tmp, QUOTE(exp), car(itr)); itr = cdr(itr);
    env_def(tmp, QUOTE(ctx), car(itr)); itr = cdr(itr);
    ret = eval_string("(list (repr exp))", tmp);
    return ret;
}

// (defun render-stmt (exp ctx) (cond (t (render-expr exp ctx))))
Expr _fun315(Expr args, Expr env)
{
    Expr ret = nil;
    Expr tmp = make_env(env);
    Expr itr = args;
    env_def(tmp, QUOTE(exp), car(itr)); itr = cdr(itr);
    env_def(tmp, QUOTE(ctx), car(itr)); itr = cdr(itr);
    ret = eval_string("(cond (t (render-expr exp ctx)))", tmp);
    return ret;
}

// (defun op? (exp name) (and (cons? exp) (eq name (car exp))))
Expr _fun316(Expr args, Expr env)
{
    Expr ret = nil;
    Expr tmp = make_env(env);
    Expr itr = args;
    env_def(tmp, QUOTE(exp), car(itr)); itr = cdr(itr);
    env_def(tmp, QUOTE(name), car(itr)); itr = cdr(itr);
    ret = eval_string("(and (cons? exp) (eq name (car exp)))", tmp);
    return ret;
}

// (defun defun? (exp) (op? exp 'defun))
Expr _fun317(Expr args, Expr env)
{
    Expr ret = nil;
    Expr tmp = make_env(env);
    Expr itr = args;
    env_def(tmp, QUOTE(exp), car(itr)); itr = cdr(itr);
    ret = eval_string("(op? exp 'defun)", tmp);
    return ret;
}

// (defun println? (exp env) (op? exp 'println))
Expr _fun318(Expr args, Expr env)
{
    Expr ret = nil;
    Expr tmp = make_env(env);
    Expr itr = args;
    env_def(tmp, QUOTE(exp), car(itr)); itr = cdr(itr);
    env_def(tmp, QUOTE(env), car(itr)); itr = cdr(itr);
    ret = eval_string("(op? exp 'println)", tmp);
    return ret;
}

// (defun mangle (name) (+ "_fun" (str (meta:expr-data name))))
Expr _fun319(Expr args, Expr env)
{
    Expr ret = nil;
    Expr tmp = make_env(env);
    Expr itr = args;
    env_def(tmp, QUOTE(name), car(itr)); itr = cdr(itr);
    ret = eval_string("(+ \"_fun\" (str (meta:expr-data name)))", tmp);
    return ret;
}

// (defun env-find (env var) (defun helper (vars vals) (cond ((nil? vars) nil) ((eq (car vars) var) vals) (t (helper (cdr vars) (cdr vals))))) (let ((pair (car env))) (let ((vars (car pair)) (vals (cdr pair))) (helper vars vals))))
Expr _fun320(Expr args, Expr env)
{
    Expr ret = nil;
    Expr tmp = make_env(env);
    Expr itr = args;
    env_def(tmp, QUOTE(env), car(itr)); itr = cdr(itr);
    env_def(tmp, QUOTE(var), car(itr)); itr = cdr(itr);
    ret = eval_string("(defun helper (vars vals) (cond ((nil? vars) nil) ((eq (car vars) var) vals) (t (helper (cdr vars) (cdr vals)))))", tmp);
    ret = eval_string("(let ((pair (car env))) (let ((vars (car pair)) (vals (cdr pair))) (helper vars vals)))", tmp);
    return ret;
}

// (defun blah (code env) (defun comp-default (stmt prefix env-name) (+ prefix "eval_string(" (escape (repr stmt)) ", " env-name ");")) (defun render-code (code ctx env) (let ((ret 'nil) (env-name (if ctx "tmp" "env")) (prefix (if ctx "ret = " ""))) (dolist (stmt code) (cond ((cons? stmt) (let* ((op (car stmt)) (args (cdr stmt)) (it (env-find env op))) (if (and nil it) (progn (push (+ "// " (repr stmt)) ret) (push (+ prefix (mangle op) "(nil, " env-name "); // TODO add args") ret)) (push (comp-default stmt prefix env-name) ret)))) ((println? stmt env) (push (+ "// " (repr stmt)) ret) (push (+ prefix "b_println(read_from_string(" (escape (repr (map std:eval (cdr stmt)))) "), env, NULL);") ret)) (t (push (comp-default stmt prefix env-name) ret)))) (nreverse ret))) (emit-tree '("" "#define LISP_IMPLEMENTATION" "#include \"runtime.hpp\"")) (defun make-bindings (args) (let ((ret nil)) (when args (= ret (map (lambda (x) (+ "env_def(tmp, QUOTE(" (str x) "), car(itr)); itr = cdr(itr);")) args)) (push "Expr itr = args;" ret)) ret)) (dolist (stmt code) (cond ((defun? stmt) (stream-put-string *print-stream* "\n// ") (emit-tree (render-stmt stmt nil)) (let (((_ name args . body) stmt)) (env-bind env name name) (let ((bindings (make-bindings args))) (emit-tree `(,(+ "Expr " (mangle name) "(Expr args, Expr env)") "{" ("Expr ret = nil;" "Expr tmp = make_env(env);" (unquote-splicing bindings) (unquote-splicing (render-code body stmt env)) "return ret;") "}"))))) (t))) (emit-tree `("" "int main(int argc, char ** argv)" "{" ("try" "{" ("system_init();" "Expr env = make_env(nil);" "system_bind_core(env);" (unquote-splicing (render-code code nil env)) "system_quit();" "return 0;") "}" "catch (Expr err)" "{" ("fprintf(stderr, \"error: %s\n\", repr(err));" "return 1;") "}") "}")))
Expr _fun324(Expr args, Expr env)
{
    Expr ret = nil;
    Expr tmp = make_env(env);
    Expr itr = args;
    env_def(tmp, QUOTE(code), car(itr)); itr = cdr(itr);
    env_def(tmp, QUOTE(env), car(itr)); itr = cdr(itr);
    ret = eval_string("(defun comp-default (stmt prefix env-name) (+ prefix \"eval_string(\" (escape (repr stmt)) \", \" env-name \");\"))", tmp);
    ret = eval_string("(defun render-code (code ctx env) (let ((ret 'nil) (env-name (if ctx \"tmp\" \"env\")) (prefix (if ctx \"ret = \" \"\"))) (dolist (stmt code) (cond ((cons? stmt) (let* ((op (car stmt)) (args (cdr stmt)) (it (env-find env op))) (if (and nil it) (progn (push (+ \"// \" (repr stmt)) ret) (push (+ prefix (mangle op) \"(nil, \" env-name \"); // TODO add args\") ret)) (push (comp-default stmt prefix env-name) ret)))) ((println? stmt env) (push (+ \"// \" (repr stmt)) ret) (push (+ prefix \"b_println(read_from_string(\" (escape (repr (map std:eval (cdr stmt)))) \"), env, NULL);\") ret)) (t (push (comp-default stmt prefix env-name) ret)))) (nreverse ret)))", tmp);
    ret = eval_string("(emit-tree '(\"\" \"#define LISP_IMPLEMENTATION\" \"#include \\\"runtime.hpp\\\"\"))", tmp);
    ret = eval_string("(defun make-bindings (args) (let ((ret nil)) (when args (= ret (map (lambda (x) (+ \"env_def(tmp, QUOTE(\" (str x) \"), car(itr)); itr = cdr(itr);\")) args)) (push \"Expr itr = args;\" ret)) ret))", tmp);
    ret = eval_string("(dolist (stmt code) (cond ((defun? stmt) (stream-put-string *print-stream* \"\\n// \") (emit-tree (render-stmt stmt nil)) (let (((_ name args . body) stmt)) (env-bind env name name) (let ((bindings (make-bindings args))) (emit-tree `(,(+ \"Expr \" (mangle name) \"(Expr args, Expr env)\") \"{\" (\"Expr ret = nil;\" \"Expr tmp = make_env(env);\" (unquote-splicing bindings) (unquote-splicing (render-code body stmt env)) \"return ret;\") \"}\"))))) (t)))", tmp);
    ret = eval_string("(emit-tree `(\"\" \"int main(int argc, char ** argv)\" \"{\" (\"try\" \"{\" (\"system_init();\" \"Expr env = make_env(nil);\" \"system_bind_core(env);\" (unquote-splicing (render-code code nil env)) \"system_quit();\" \"return 0;\") \"}\" \"catch (Expr err)\" \"{\" (\"fprintf(stderr, \\\"error: %s\\n\\\", repr(err));\" \"return 1;\") \"}\") \"}\"))", tmp);
    return ret;
}

int main(int argc, char ** argv)
{
    try
    {
        system_init();
        Expr env = make_env(nil);
        system_bind_core(env);
        eval_string("(load-file \"std.lisp\")", env);
        eval_string("(defun put-string (str) (stream-put-string *print-stream* str))", env);
        eval_string("(defun line-tree-to-stream (tree out indent) (defun emit (str) (stream-put-string out str)) (defun helper (tree prefix) (dolist (item tree) (cond ((string? item) (emit prefix) (emit item) (emit \"\\n\")) (t (helper item (+ indent prefix)))))) (helper tree \"\"))", env);
        eval_string("(defun emit-tree (tree) (line-tree-to-stream tree *print-stream* \"    \"))", env);
        eval_string("(defun render-expr (exp ctx) (list (repr exp)))", env);
        eval_string("(defun render-stmt (exp ctx) (cond (t (render-expr exp ctx))))", env);
        eval_string("(defun op? (exp name) (and (cons? exp) (eq name (car exp))))", env);
        eval_string("(defun defun? (exp) (op? exp 'defun))", env);
        eval_string("(defun println? (exp env) (op? exp 'println))", env);
        eval_string("(defun mangle (name) (+ \"_fun\" (str (meta:expr-data name))))", env);
        eval_string("(defun env-find (env var) (defun helper (vars vals) (cond ((nil? vars) nil) ((eq (car vars) var) vals) (t (helper (cdr vars) (cdr vals))))) (let ((pair (car env))) (let ((vars (car pair)) (vals (cdr pair))) (helper vars vals))))", env);
        eval_string("(defun blah (code env) (defun comp-default (stmt prefix env-name) (+ prefix \"eval_string(\" (escape (repr stmt)) \", \" env-name \");\")) (defun render-code (code ctx env) (let ((ret 'nil) (env-name (if ctx \"tmp\" \"env\")) (prefix (if ctx \"ret = \" \"\"))) (dolist (stmt code) (cond ((cons? stmt) (let* ((op (car stmt)) (args (cdr stmt)) (it (env-find env op))) (if (and nil it) (progn (push (+ \"// \" (repr stmt)) ret) (push (+ prefix (mangle op) \"(nil, \" env-name \"); // TODO add args\") ret)) (push (comp-default stmt prefix env-name) ret)))) ((println? stmt env) (push (+ \"// \" (repr stmt)) ret) (push (+ prefix \"b_println(read_from_string(\" (escape (repr (map std:eval (cdr stmt)))) \"), env, NULL);\") ret)) (t (push (comp-default stmt prefix env-name) ret)))) (nreverse ret))) (emit-tree '(\"\" \"#define LISP_IMPLEMENTATION\" \"#include \\\"runtime.hpp\\\"\")) (defun make-bindings (args) (let ((ret nil)) (when args (= ret (map (lambda (x) (+ \"env_def(tmp, QUOTE(\" (str x) \"), car(itr)); itr = cdr(itr);\")) args)) (push \"Expr itr = args;\" ret)) ret)) (dolist (stmt code) (cond ((defun? stmt) (stream-put-string *print-stream* \"\\n// \") (emit-tree (render-stmt stmt nil)) (let (((_ name args . body) stmt)) (env-bind env name name) (let ((bindings (make-bindings args))) (emit-tree `(,(+ \"Expr \" (mangle name) \"(Expr args, Expr env)\") \"{\" (\"Expr ret = nil;\" \"Expr tmp = make_env(env);\" (unquote-splicing bindings) (unquote-splicing (render-code body stmt env)) \"return ret;\") \"}\"))))) (t))) (emit-tree `(\"\" \"int main(int argc, char ** argv)\" \"{\" (\"try\" \"{\" (\"system_init();\" \"Expr env = make_env(nil);\" \"system_bind_core(env);\" (unquote-splicing (render-code code nil env)) \"system_quit();\" \"return 0;\") \"}\" \"catch (Expr err)\" \"{\" (\"fprintf(stderr, \\\"error: %s\\n\\\", repr(err));\" \"return 1;\") \"}\") \"}\")))", env);
        eval_string("(let ((code (read-all-from-stream *read-stream*)) (env (make-env *env*))) (blah code env))", env);
        system_quit();
        return 0;
    }
    catch (Expr err)
    {
        fprintf(stderr, "error: %s\n", repr(err));
        return 1;
    }
}
