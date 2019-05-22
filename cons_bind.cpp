
#include "lisp.hpp"

static Expr b_cons(Expr args, Expr env, void * user)
{
    Expr a = nil;
    Expr b = nil;
    if (!unpack_all_args(args, "xx", &a, &b))
    {
        return ERROR("illegal arguments -- CONS");
    }
    return f_cons(a, b);
}

static Expr b_car(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    if (!unpack_all_args(args, "x", &exp))
    {
        return ERROR("illegal arguments -- CAR");
    }
    return f_car(exp);
}

static Expr b_cdr(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    if (!unpack_all_args(args, "x", &exp))
    {
        return ERROR("illegal arguments -- CDR");
    }
    return f_cdr(exp);
}

static Expr b_rplaca(Expr args, Expr env, void * user)
{
    Expr a = nil;
    Expr b = nil;
    if (!unpack_all_args(args, "xx", &a, &b))
    {
        return ERROR("illegal arguments -- RPLACA");
    }
    set_car(a, b);
    return a;
}

static Expr b_rplacd(Expr args, Expr env, void * user)
{
    Expr a = nil;
    Expr b = nil;
    if (!unpack_all_args(args, "xx", &a, &b))
    {
        return ERROR("illegal arguments -- RPLACD");
    }
    set_cdr(a, b);
    return a;
}

#define FUN2(sym, name) env_def(env, QUOTE(sym), make_builtin_fun(name, NULL))
#define FUN1(sym) FUN2(sym, b_##sym)

void cons_install(Expr env)
{
    FUN1(cons);
    FUN1(car);
    FUN1(cdr);

    FUN1(rplaca);
    FUN1(rplacd);
}

#undef FUN1
#undef FUN2
