
#include "lisp.hpp"

#if !NIL_IS_MACRO
Expr nil = 0;
#endif

#if ENABLE_SMART_EXPR

Expr::Expr()
{
}

Expr::Expr(U64 bits) : bits(bits)
{
}

Expr::~Expr()
{
}

Expr::operator Bool() const
{
    return bits != 0;
}

#endif

#if !INLINE_EXPR_FUNS

Expr make_expr(U64 type, U64 data)
{
    return (data << TYPE_BITS) | (type & TYPE_MASK);
}

U64 expr_type(Expr exp)
{
    return exp & TYPE_MASK;
}

U64 expr_data(Expr exp)
{
    return exp >> TYPE_BITS;
}

Bool is_nil(Expr exp)
{
    return exp == nil;
}

#endif
