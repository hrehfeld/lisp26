
#include "lisp.hpp"

#if BIND_ENV

static Expr fun_make_env(Expr args, Expr env, void * user)
{
    Expr arg1 = nil;
    int const nargs = unpack_args(args, "x", &arg1);

    if (nargs == 0)
    {
        return make_env(nil);
    }
    else if (nargs == 1)
    {
        return make_env(arg1);
    }
    else
    {
        return ERROR("usage: (make-env [outer])");
    }
}

static Expr fun_env_def(Expr args, Expr env, void * user)
{
    Expr arg1 = nil;
    Expr arg2 = nil;
    Expr arg3 = nil;
    int const nargs = unpack_args(args, "xxx", &arg1, &arg2, &arg3);

    if (nargs == 2)
    {
        env_def(env, arg1, arg2);
        return nil;
    }
    else if (nargs == 3)
    {
        env_def(arg1, arg2, arg3);
        return nil;
    }
    else
    {
        /* TODO change this */
        return ERROR("usage: (env-def [env] var val(");
    }
}

static Expr fun_env_del(Expr args, Expr env, void * user)
{
    Expr arg1 = nil;
    Expr arg2 = nil;
    int const nargs = unpack_args(args, "xx", &arg1, &arg2);

    if (nargs == 1)
    {
        env_del(env, arg1);
        return nil;
    }
    else if (nargs == 2)
    {
        env_del(arg1, arg2);
        return nil;
    }
    else
    {
        return ERROR("usage: (env-del [env] var)");
    }
}

static Expr fun_env_has(Expr args, Expr env, void * user)
{
    Expr arg1 = nil;
    Expr arg2 = nil;
    int const nargs = unpack_args(args, "xx", &arg1, &arg2);

    if (nargs == 1)
    {
        return make_truth(env_has(env, arg1));
    }
    else if (nargs == 2)
    {
        return make_truth(env_has(arg1, arg2));
    }
    else
    {
        return ERROR("usage: (env-has [env] var)");
    }
}

static Expr fun_env_set(Expr args, Expr env, void * user)
{
    Expr arg1 = nil;
    Expr arg2 = nil;
    Expr arg3 = nil;
    int const nargs = unpack_args(args, "xxx", &arg1, &arg2, &arg3);

    if (nargs == 2)
    {
        return env_set(env, arg1, arg2);
    }
    else if (nargs == 3)
    {
        return env_set(arg1, arg2, arg3);
    }
    else
    {
        /* TODO change this */
        return ERROR("usage: (env-set [env] var val(");
    }
}

static Expr fun_env_lookup(Expr args, Expr env, void * user)
{
    Expr arg1 = nil;
    Expr arg2 = nil;
    int const nargs = unpack_args(args, "xx", &arg1, &arg2);

    if (nargs == 1)
    {
        return env_lookup(env, arg1);
    }
    else if (nargs == 2)
    {
        return env_lookup(arg1, arg2);
    }
    else
    {
        return ERROR("usage: (env-lookup [env] var)");
    }
}

static Expr fun_env_outer(Expr args, Expr env, void * user)
{
    Expr arg1 = nil;
    int const nargs = unpack_args(args, "x", &arg1);

    if (nargs == 0)
    {
        return env_outer(env);
    }
    else if (nargs == 1)
    {
        return env_outer(arg1);
    }
    else
    {
        return ERROR("usage: (env-outer [env])");
    }
}

void env_install(Expr env)
{
    env_def(env, QUOTE(make-env       ), make_builtin_fun(fun_make_env  , NULL));

    env_def(env, QUOTE(core:env-def   ), make_builtin_fun(fun_env_def   , NULL));
    env_def(env, QUOTE(core:env-del   ), make_builtin_fun(fun_env_del   , NULL));
    env_def(env, QUOTE(core:env-has   ), make_builtin_fun(fun_env_has   , NULL));
    env_def(env, QUOTE(core:env-set   ), make_builtin_fun(fun_env_set   , NULL));
    env_def(env, QUOTE(core:env-lookup), make_builtin_fun(fun_env_lookup, NULL));
    env_def(env, QUOTE(core:env-outer ), make_builtin_fun(fun_env_outer , NULL));

    /* TODO clean up API */

    env_alias(env, QUOTE(env-def), QUOTE(core:env-def));
    env_alias(env, QUOTE(env-del), QUOTE(core:env-del));
    env_alias(env, QUOTE(env-has), QUOTE(core:env-has));
    env_alias(env, QUOTE(env-lookup), QUOTE(core:env-lookup));
    env_alias(env, QUOTE(env-outer ), QUOTE(core:env-outer ));

    env_alias(env, SYM_env_bind  , QUOTE(core:env-def));
    env_alias(env, SYM_env_unbind, QUOTE(core:env-del));
    env_alias(env, SYM_env_set   , QUOTE(core:env-set));

    env_alias(env, QUOTE(core:env-bind), QUOTE(core:env-def));
    env_alias(env, QUOTE(core:env-unbind), QUOTE(core:env-del));
}

#else

void env_install(Expr env)
{
}

#endif
