
#include "lisp.hpp"

static U64 num_pointers = 0;
static void * pointers[MAX_POINTERS];

Bool is_pointer(Expr exp)
{
    return exp == nil ||
        expr_type(exp) == TYPE_POINTER ||
        expr_type(exp) == TYPE_FIXPTR;
}

Expr make_pointer(void * ptr)
{
    //printf("make-pointer %p\n", ptr);

    U64 const value = (U64) ptr;
    if ((value & ~TYPE_MASK) == value)
    {
        return value | TYPE_FIXPTR;
    }

    if (num_pointers < MAX_POINTERS)
    {
        U64 const index = num_pointers;
        pointers[index] = ptr;
        ++num_pointers;
        return make_expr(TYPE_POINTER, index);
    }

    return ERROR("cannot make pointer from %p", ptr);
}

void * pointer_value(Expr exp)
{
    if (exp == nil)
    {
        return NULL;
    }

    if (expr_type(exp) == TYPE_FIXPTR)
    {
        return (void *) (exp & ~TYPE_MASK);
    }

    if (expr_type(exp) == TYPE_POINTER)
    {
        U64 const index = expr_data(exp);
        if (index >= num_pointers)
        {
            ERROR("illegal pointer index %" PRIu64, index);
        }

        return pointers[index];
    }

    ERROR("expression is not a pointer");
    return NULL;
}
