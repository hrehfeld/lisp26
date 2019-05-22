
#define LISP_IMPLEMENTATION
#include "runtime.hpp"

int main(int argc, char ** argv)
{
    try
    {
        system_init();
        Expr env = make_env(nil);
        system_bind_core(env);
        eval_string("(println 'hello 'lisp)", env);
        system_quit();
        return 0;
    }
    catch (Expr err)
    {
        fprintf(stderr, "error: %s\n", repr(err));
        return 1;
    }
}
