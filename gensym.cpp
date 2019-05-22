
#include "lisp.hpp"

#if ENABLE_GENSYM

void gensym_init(Gensym * gensym)
{
    gensym->counter = 0;
}

void gensym_quit()
{
}

Bool is_gensym(Expr exp)
{
    return expr_type(exp) == TYPE_GENSYM;
}

Expr make_gensym(Gensym * gensym)
{
    return make_expr(TYPE_GENSYM, gensym->counter++);
}

void p_gensym(PrintFun rec, Expr out, Expr exp)
{
    U64 const num = expr_data(exp);
    stream_put_cstring(out, "#:G");
    stream_put_u64(out, num);
}

#endif
