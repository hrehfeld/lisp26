
#define LISP_IMPLEMENTATION
#include "runtime.hpp"

// (defun hello nil (println 'hello 'lisp))
Expr _fun335(Expr args, Expr env)
{
    Expr ret = nil;
    Expr tmp = make_env(env);
    ret = eval_string("(println 'hello 'lisp)", tmp);
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
        eval_string("(defun hello nil (println 'hello 'lisp))", env);
        eval_string("(hello)", env);
        system_quit();
        return 0;
    }
    catch (Expr err)
    {
        fprintf(stderr, "error: %s\n", repr(err));
        return 1;
    }
}
