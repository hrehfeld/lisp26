
#include "lisp.hpp"

static ApplyFun _builtin_callbacks[MAX_BUILTINS];
static void *   _builtin_userdata[MAX_BUILTINS];
static U64      _num_builtins = 0;

Bool is_builtin_fun(Expr exp)
{
    return expr_type(exp) == TYPE_BUILTIN_FUN;
}

Bool is_builtin_mac(Expr exp)
{
    return expr_type(exp) == TYPE_BUILTIN_MAC;
}

static Expr _make_builtin(U64 type, ApplyFun apply, void * user)
{
    if (_num_builtins >= MAX_BUILTINS)
    {
        return ERROR("too many builtins");
    }

    U64 const index = _num_builtins++;
    _builtin_callbacks[index] = apply;
    _builtin_userdata [index] = user;
    return make_expr(type, index);
}

Expr make_builtin_fun(ApplyFun apply, void * user)
{
    return _make_builtin(TYPE_BUILTIN_FUN, apply, user);
}

Expr make_builtin_mac(ApplyFun apply, void * user)
{
    return _make_builtin(TYPE_BUILTIN_MAC, apply, user);
}

static Bool _is_builtin(Expr exp)
{
    return is_builtin_fun(exp) || is_builtin_mac(exp);
}

static U64 builtin_index(Expr exp)
{
    ASSERT(_is_builtin(exp));

    U64 const index = expr_data(exp);
    if (index >= _num_builtins)
    {
        ERROR("illegal builtin index %" PRIu64, index);
    }
    return index;
}

ApplyFun builtin_apply(Expr exp)
{
    U64 const index = builtin_index(exp);
    return _builtin_callbacks[index];
}

void * builtin_user(Expr exp)
{
    U64 const index = builtin_index(exp);
    return _builtin_userdata[index];
}
