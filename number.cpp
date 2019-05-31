
#include "lisp.hpp"

Expr make_number(I64 value)
{
#if ENABLE_FIXNUM
    return make_fixnum(value);
#else
    char tmp[24]; // worst case is -9223372036854775808
    sprintf(tmp, "%" PRIi64, value);
    return intern(tmp); /* not a number, but at least looks like one */
#endif
}

I64 number_value(Expr exp)
{
#if ENABLE_FIXNUM
    if (is_fixnum(exp))
    {
        return fixnum_value(exp);
    }
#endif

#if ENABLE_BIGNUM
    if (is_bignum(exp))
    {
        return bignum_value(exp);
    }
#endif

    ERROR("not a number");
    return -1;
}

#if ENABLE_BIGNUM

static Expr _normalize(Expr num)
{
#if ENABLE_FIXNUM
    if (is_bignum(num) && bignum_is_fixnum(num))
    {
        num = make_fixnum(num);
    }
#endif
    return num;
}

#endif

Bool number_equal(Expr a, Expr b)
{
    if (is_fixnum(a) && is_fixnum(b))
    {
        return a == b;
    }

    ERROR("cannot compare %s and %s", repr(a), repr(b));
    return 0;
}

Expr number_neg(Expr a)
{
#if ENABLE_FIXNUM
    if (is_fixnum(a))
    {
	    //TODO: convert smallest/largest fixnum to bignum
	    return fixnum_neg(a);
    }
#endif

#if ENABLE_BIGNUM
    if (is_bignum(a)) {
	    return _normalize(bignum_neg(a));
    }
#endif
    return ERROR("cannot neg %s", repr(a));
}


Expr number_add(Expr a, Expr b)
{
#if ENABLE_BIGNUM
//    if (is_fixnum(a) && is_fixnum(b))
//    {
//        return fixnum_add(a, b);
//    }

#if ENABLE_FIXNUM
    if (is_fixnum(a))
    {
        a = fixnum_to_bignum(a);
    }

    if (is_fixnum(b))
    {
        b = fixnum_to_bignum(b);
    }
#endif

    return _normalize(bignum_add(a, b));
#elif ENABLE_FIXNUM
    return fixnum_add(a, b);
#else
    ERROR("cannot add %s %s", repr(a), repr(b));
    return nil;
#endif
}

Expr number_sub(Expr a, Expr b)
{
#if ENABLE_BIGNUM
//    if (is_fixnum(a) && is_fixnum(b))
//    {
//        return fixnum_sub(a, b);
//    }

#if ENABLE_FIXNUM
    if (is_fixnum(a))
    {
        a = fixnum_to_bignum(a);
    }

    if (is_fixnum(b))
    {
        b = fixnum_to_bignum(b);
    }
#endif

    return _normalize(bignum_sub(a, b));
#elif ENABLE_FIXNUM
    return fixnum_sub(a, b);
#else
    ERROR("cannot sub %s %s", repr(a), repr(b));
    return nil;
#endif
}

Expr number_mul(Expr a, Expr b)
{
#if ENABLE_BIGNUM
//    if (is_fixnum(a) && is_fixnum(b))
//    {
//        return fixnum_mul(a, b);
//    }

#if ENABLE_FIXNUM
    if (is_fixnum(a))
    {
        a = fixnum_to_bignum(a);
    }

    if (is_fixnum(b))
    {
        b = fixnum_to_bignum(b);
    }
#endif

    return _normalize(bignum_mul(a, b));
#elif ENABLE_FIXNUM
    return fixnum_mul(a, b);
#else
    ERROR("cannot mul %s %s", repr(a), repr(b));
    return nil;
#endif
}

Expr number_div(Expr a, Expr b)
{
#if ENABLE_FIXNUM && ENABLE_FLOAT
    // TODO this is a bit fishy
    if (is_fixnum(a))
    {
        a = make_float(fixnum_value(a));
    }

    if (is_fixnum(b))
    {
        b = make_float(fixnum_value(b));
    }

    return float_div(a, b);
#elif ENABLE_FIXNUM
    return fixnum_div(a, b); // TODO maybe better to throw an error
#else
    ERROR("cannot div %s %s", repr(a), repr(b));
    return nil;
#endif
}

Expr number_mod(Expr a, Expr b)
{
#if ENABLE_FIXNUM
    return fixnum_mod(a, b);
#else
    ERROR("cannot mod %s %s", repr(a), repr(b));
    return nil;
#endif
}

static Expr b_number_add(Expr args, Expr env, void * user)
{
    return number_add(car(args), cadr(args));
}

static Expr b_number_sub(Expr args, Expr env, void * user)
{
    return number_sub(car(args), cadr(args));
}

static Expr b_number_mul(Expr args, Expr env, void * user)
{
    return number_mul(car(args), cadr(args));
}

static Expr b_number_div(Expr args, Expr env, void * user)
{
    return number_div(car(args), cadr(args));
}

void number_bind(Expr env)
{
    env_def(env, QUOTE(number:add), make_builtin_fun(b_number_add, NULL));
    env_def(env, QUOTE(number:sub), make_builtin_fun(b_number_sub, NULL));
    env_def(env, QUOTE(number:mul), make_builtin_fun(b_number_mul, NULL));
    env_def(env, QUOTE(number:div), make_builtin_fun(b_number_div, NULL));
}
