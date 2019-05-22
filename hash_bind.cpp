
#include "lisp.hpp"

#if BIND_HASH && !ENABLE_HASH
#define BIND_HASH 0
#endif

#if BIND_HASH

static Expr b_eq_hash(Expr args, Expr env, void * user)
{
    return eq_hash(car(args));
}

static Expr b_equal_hash(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    if (!unpack_all_args(args, "x", &exp))
    {
        return ERROR("illegal arguments -- EQUAL-HASH");
    }
    return equal_hash(exp);
}

static Expr b_make_hash(Expr args, Expr env, void * user)
{
    return make_hash();
}

static Expr b_hash_has(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    Expr key = nil;
    if (!unpack_all_args(args, "xx", &exp, &key))
    {
        return ERROR("illegal arguments -- HASH-HAS");
    }
    return hash_has(exp, key);
}

static Expr b_hash_get(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    Expr key = nil;
    if (!unpack_all_args(args, "xx", &exp, &key))
    {
        return ERROR("illegal arguments -- HASH-GET");
    }
    return hash_get(exp, key);
}

static Expr b_hash_put(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    Expr key = nil;
    Expr val = nil;
    if (!unpack_all_args(args, "xxx", &exp, &key, &val))
    {
        return ERROR("illegal arguments -- HASH-PUT");
    }
    hash_put(exp, key, val);
    return nil;
}

void hash_bind(Expr env)
{
    env_def(env, QUOTE(eq-hash   ), make_builtin_fun(b_eq_hash   , NULL));
    env_def(env, QUOTE(equal-hash), make_builtin_fun(b_equal_hash, NULL));

    env_def(env, QUOTE(make-hash), make_builtin_fun(b_make_hash, NULL));
    env_def(env, QUOTE(hash-has ), make_builtin_fun(b_hash_has , NULL));
    env_def(env, QUOTE(hash-get ), make_builtin_fun(b_hash_get , NULL));
    env_def(env, QUOTE(hash-put ), make_builtin_fun(b_hash_put , NULL));
}

#else

void hash_bind(Expr env)
{
}

#endif
