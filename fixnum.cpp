
#include "lisp.hpp"

#if ENABLE_FIXNUM

static Expr fold_relop(Bool (* op)(Expr a, Expr b), Expr args)
{
    if (!args)
    {
        return ERROR("too few arguments");
    }

    Expr prev = car(args);
    for (Expr iter = cdr(args); iter; iter = cdr(iter))
    {
        Expr curr = car(iter);
        if (!op(prev, curr))
        {
            return nil;
        }
        prev = curr;
    }
    return SYM_t;
}

static Expr b_fixnum_add(Expr args, Expr env, void * user)
{
    Expr ret = make_fixnum(0);
    for (Expr iter = args; iter; iter = cdr(iter))
    {
        ret = fixnum_add(ret, car(iter));
    }
    return ret;
}

static Expr b_fixnum_sub(Expr args, Expr env, void * user)
{
    Expr a = nil;
    Expr b = nil;
    if (unpack_args(args, "xx", &a, &b) < 1)
    {
        return ERROR("illegal arguments");
    }
    if (b)
    {
        return fixnum_sub(a, b);
    }
    else
    {
        return fixnum_sub(make_fixnum(0), a);
    }
}

static Expr b_fixnum_mul(Expr args, Expr env, void * user)
{
    Expr ret = make_fixnum(1);
    for (Expr iter = args; iter; iter = cdr(iter))
    {
        ret = fixnum_mul(ret, car(iter));
    }
    return ret;
}

static Expr b_fixnum_div(Expr args, Expr env, void * user)
{
    Expr a = nil;
    Expr b = nil;
    if (!unpack_all_args(args, "xx", &a, &b))
    {
        return ERROR("illegal arguments");
    }
    return fixnum_div(a, b);
}

static Expr b_fixnum_mod(Expr args, Expr env, void * user)
{
    Expr a = nil;
    Expr b = nil;
    if (!unpack_all_args(args, "xx", &a, &b))
    {
        return ERROR("illegal arguments");
    }
    return fixnum_mod(a, b);
}

static Expr b_fixnum_rem(Expr args, Expr env, void * user)
{
    Expr a = nil;
    Expr b = nil;
    if (!unpack_all_args(args, "xx", &a, &b))
    {
        return ERROR("illegal arguments");
    }
    return fixnum_rem(a, b);
}

static Expr b_fixnum_eq(Expr args, Expr env, void * user)
{
    return fold_relop(fixnum_eq, args);
}

static Expr b_fixnum_ne(Expr args, Expr env, void * user)
{
    return fold_relop(fixnum_ne, args);
}

static Expr b_fixnum_lt(Expr args, Expr env, void * user)
{
    return fold_relop(fixnum_lt, args);
}

static Expr b_fixnum_le(Expr args, Expr env, void * user)
{
    return fold_relop(fixnum_le, args);
}

static Expr b_fixnum_gt(Expr args, Expr env, void * user)
{
    return fold_relop(fixnum_gt, args);
}

static Expr b_fixnum_ge(Expr args, Expr env, void * user)
{
    return fold_relop(fixnum_ge, args);
}

void bind_fixnum(Expr env)
{
    env_def(env, QUOTE(fixnum:add), make_builtin_fun(b_fixnum_add, NULL));
    env_def(env, QUOTE(fixnum:sub), make_builtin_fun(b_fixnum_sub, NULL));
    env_def(env, QUOTE(fixnum:mul), make_builtin_fun(b_fixnum_mul, NULL));
    env_def(env, QUOTE(fixnum:div), make_builtin_fun(b_fixnum_div, NULL));
    env_def(env, QUOTE(fixnum:mod), make_builtin_fun(b_fixnum_mod, NULL));
    env_def(env, QUOTE(fixnum:rem), make_builtin_fun(b_fixnum_rem, NULL));
    env_def(env, QUOTE(fixnum:eq) , make_builtin_fun(b_fixnum_eq , NULL));
    env_def(env, QUOTE(fixnum:ne) , make_builtin_fun(b_fixnum_ne , NULL));
    env_def(env, QUOTE(fixnum:lt) , make_builtin_fun(b_fixnum_lt , NULL));
    env_def(env, QUOTE(fixnum:le) , make_builtin_fun(b_fixnum_le , NULL));
    env_def(env, QUOTE(fixnum:gt) , make_builtin_fun(b_fixnum_gt , NULL));
    env_def(env, QUOTE(fixnum:ge) , make_builtin_fun(b_fixnum_ge , NULL));
}

Bool is_fixnum(Expr exp)
{
    return expr_type(exp) == TYPE_FIXNUM;
}

Expr make_fixnum(I64 value)
{
    ASSERT_EXPR(value >= FIXNUM_MINVAL);
    ASSERT_EXPR(value <= FIXNUM_MAXVAL);

    /* TODO probably no need to mask off the sign
       bits, as they get shifted out by make_expr */
    U64 const data = i64_as_u64(value) & DATA_MASK;
    return make_expr(TYPE_FIXNUM, data);
}

I64 fixnum_value(Expr exp)
{
    ASSERT(is_fixnum(exp));

    U64 data = expr_data(exp);

    if (data & SIGN_MASK)
    {
        data |= ALL_BITS << DATA_BITS;
    }

    return u64_as_i64(data);
}

Bool fixnum_maybe_add(Expr a, Expr b, Expr * out)
{
    I64 const x = fixnum_value(a);
    I64 const y = fixnum_value(b);
    if ((y > 0 && x > FIXNUM_MAXVAL - y) ||
        (y < 0 && x < FIXNUM_MINVAL - y))
    {
        return 0;
    }
    *out = make_fixnum(x + y);
    return 1;
}

Bool fixnum_maybe_sub(Expr a, Expr b, Expr * out)
{
    I64 const x = fixnum_value(a);
    I64 const y = fixnum_value(b);
    if ((y < 0 && x > FIXNUM_MAXVAL + y) ||
        (y > 0 && x < FIXNUM_MINVAL + y))
    {
        return 0;
    }
    *out = make_fixnum(x - y);
    return 1;
}

Bool fixnum_maybe_mul(Expr a, Expr b, Expr * out)
{
    I64 const x = fixnum_value(a);
    I64 const y = fixnum_value(b);
    if ((x > FIXNUM_MAXVAL / y) ||
        (x < FIXNUM_MINVAL / y))
    {
        return 0;
    }
    *out = make_fixnum(x * y);
    return 1;
}

Expr fixnum_add(Expr a, Expr b)
{
    // TODO
//    Expr c;
//    if (!fixnum_maybe_add(a, b, &c))
//    {
//        ERROR("overflow");
//    }
//    return c;
    I64 const x = fixnum_value(a);
    I64 const y = fixnum_value(b);
    I64 const z = x + y;
    return make_fixnum(z);
}

Expr fixnum_sub(Expr a, Expr b)
{
    I64 const x = fixnum_value(a);
    I64 const y = fixnum_value(b);
    I64 const z = x - y;
    return make_fixnum(z);
}

Expr fixnum_mul(Expr a, Expr b)
{
    I64 const x = fixnum_value(a);
    I64 const y = fixnum_value(b);
    I64 const z = x * y;
    return make_fixnum(z);
}

Expr fixnum_div(Expr a, Expr b)
{
    I64 const x = fixnum_value(a);
    I64 const y = fixnum_value(b);
#if CHECK_DIV0
    if (y == 0)
    {
        return ERROR("division by zero");
    }
#endif
    I64 const z = x / y;
    return make_fixnum(z);
}

Expr fixnum_mod(Expr a, Expr b)
{
    I64 const x = fixnum_value(a);
    I64 const y = fixnum_value(b);
#if CHECK_DIV0
    if (!y)
    {
        return ERROR("division by zero");
    }
#endif
    I64 const z = x % y;
    return make_fixnum(z);
}

Expr fixnum_rem(Expr a, Expr b)
{
    I64 const x = fixnum_value(a);
    I64 const y = fixnum_value(b);
#if CHECK_DIV0
    if (!y)
    {
        return ERROR("division by zero");
    }
#endif
    I64 z = x % y;
    z += (z < 0) * y;
    return make_fixnum(z);
}

Expr fixnum_xor(Expr a, Expr b)
{
    I64 const x = fixnum_value(a);
    I64 const y = fixnum_value(b);
    I64 z = x ^ y;
    return make_fixnum(z);
}

Bool fixnum_eq(Expr a, Expr b)
{
    return a == b;
}

Bool fixnum_ne(Expr a, Expr b)
{
    return a != b;
}

Bool fixnum_lt(Expr a, Expr b)
{
    return fixnum_value(a) < fixnum_value(b);
}

Bool fixnum_le(Expr a, Expr b)
{
    return fixnum_value(a) <= fixnum_value(b);
}

Bool fixnum_gt(Expr a, Expr b)
{
    return fixnum_value(a) > fixnum_value(b);
}

/*

(defun fixnum-ge (a b)
  (declare Bool fixnum-ge)
  (>= (fixnum-value a) (fixnum-value b)))

*/

Bool fixnum_ge(Expr a, Expr b)
{
    return fixnum_value(a) >= fixnum_value(b);
}

#endif
