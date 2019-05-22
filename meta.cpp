
#include "lisp.hpp"

static Expr b_expr_type(Expr args, Expr env, void * user)
{
    return make_number(expr_type(car(args)));
}

static Expr b_expr_data(Expr args, Expr env, void * user)
{
    return make_number(expr_data(car(args)));
}

void meta_install(Expr env)
{
    /* TODO move to meta package? */
    env_def(env, QUOTE(meta:expr-type), make_builtin_fun(b_expr_type, NULL));
    env_def(env, QUOTE(meta:expr-data), make_builtin_fun(b_expr_data, NULL));
}
