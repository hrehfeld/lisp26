
#include "lisp.hpp"
#include "cons_impl.hpp"

void cons_init()
{
    cb_init(&g_sys.cons);
}

void cons_quit()
{
    cb_free(&g_sys.cons);
}

void cons_gc(U64 num_roots, Expr ** roots)
{
    cb_gc(&g_sys.cons, num_roots, roots);
}

Bool is_cons(Expr exp)
{
    return expr_type(exp) == TYPE_CONS;
}

Expr cons(Expr a, Expr b)
{
    return cb_cons(&g_sys.cons, a, b);
}

Expr car(Expr exp)
{
    return *cb_car(&g_sys.cons, exp);
}

Expr cdr(Expr exp)
{
    return *cb_cdr(&g_sys.cons, exp);
}

void set_car(Expr exp, Expr val)
{
    *cb_car(&g_sys.cons, exp) = val;
}

void set_cdr(Expr exp, Expr val)
{
    *cb_cdr(&g_sys.cons, exp) = val;
}

/* **** */

Expr make_cons(Expr a, Expr b)
{
    return cons(a, b);
}

Expr f_car(Expr exp)
{
    return car(exp);
}

Expr f_cdr(Expr exp)
{
    return cdr(exp);
}

Expr * gc_car(Expr exp)
{
    return cb_car(&g_sys.cons, exp);
}

Expr * gc_cdr(Expr exp)
{
    return cb_cdr(&g_sys.cons, exp);
}
