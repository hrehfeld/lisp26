
#include "lisp.hpp"

#if ENABLE_FLOAT

Bool is_float(Expr exp)
{
    return expr_type(exp) == TYPE_FLOAT;
}

Expr make_float(F32 value)
{
    return make_expr(TYPE_FLOAT, f32_as_u32(value));
}

F32 float_value(Expr exp)
{
    ASSERT(is_float(exp));
    return u32_as_f32((U32) expr_data(exp));
}

Expr float_div(Expr a, Expr b)
{
    return make_float(float_value(a) / float_value(b));
}

void p_float(PrintFun rec, Expr out, Expr exp)
{
    stream_put_f32(out, float_value(exp));
}

#endif
