
#include "lisp.hpp"

#if ENV_USE_HASH && !ENABLE_HASH
#define ENV_USE_HASH 0
#endif

#if PROFILE_LOOKUP
#include <algorithm>
F64 _lookup_time = 0.0;
U64 _lookup_call = 0;
U64 _max_vars    = 0;
#endif

#if ENV_USE_HASH

static Expr env_hash(Expr env)
{
    return car(env);
}

Expr make_env(Expr outer)
{
    return cons(make_hash(), outer);
}

Expr env_outer(Expr env)
{
    return cdr(env);
}

Expr env_vars(Expr env)
{
    /* TODO remove from env API */
    return ERROR("not implemented");
}

void env_def(Expr env, Expr var, Expr val)
{
    //printf("ENV_BIND %s %s\n", repr(var), repr(val));
    Expr const hash = env_hash(env);
    if (!is_hash(hash))
    {
        println(env);
    }
    hash_put(hash, var, val);
    //println(hash);
}

void env_del(Expr env, Expr var)
{
    /* TODO need a way to remove keys from hash maps */
    ERROR("env-unbind not implemented!");
}

Bool env_has(Expr env, Expr var)
{
    while (env)
    {
        Expr const hash = env_hash(env);
        if (hash_has(hash, var))
        {
            return 1;
        }
        env = env_outer(env);
    }
    return 0;
}

void env_set(Expr env, Expr var, Expr val)
{
    while (env)
    {
        Expr const hash = env_hash(env);
        if (hash_has(hash, var)) /* TODO refactor into hash_maybe_put? */
        {
            hash_put(hash, var, val);
            return;
        }
        else
        {
            env = env_outer(env);
        }
    }

    ERROR("unbound variable %s", repr(var));
}

Expr env_lookup(Expr env, Expr var)
{
    Expr val;
    if (env_maybe_lookup(env, var, &val))
    {
        return val;
    }
    return ERROR("unbound variable %s", repr(var));
}

Bool env_maybe_lookup(Expr env, Expr var, Expr * val)
{
#if PROFILE_LOOKUP
    Profiler prof(&_lookup_time, &_lookup_call);
#endif
    while (env)
    {
        Expr const hash = env_hash(env);
        if (hash_maybe_get(hash, var, val))
        {
            return 1;
        }
        else
        {
            env = env_outer(env);
        }
    }
    return 0;
}

#else

static Expr env_find(Expr env, Expr var)
{
    Expr const pair = car(env);
    Expr vars = car(pair);
    Expr vals = cdr(pair);
    while (vars)
    {
        if (car(vars) == var)
        {
            return vals;
        }
        vars = cdr(vars);
        vals = cdr(vals);
    }
    return nil;
}

static Expr env_find_global(Expr env, Expr var)
{
    while (env)
    {
        Expr const vals = env_find(env, var);
        if (vals)
        {
            return vals;
        }
        else
        {
            env = env_outer(env);
        }
    }
    return nil;
}

Expr make_env(Expr outer)
{
    //rformat("MAKE-ENV {}\n", outer);
	// env: (( names . values) . outer-env)
    return cons(cons(nil, nil), outer);
}

Expr env_vars(Expr env)
{
    return caar(env);
}

void _env_set_vars(Expr env, Expr vars)
{
    Expr pair = car(env);
    set_car(pair, cdr(vars));
}

Expr env_vals(Expr env)
{
    return cdar(env);
}

void _env_set_vals(Expr env, Expr vals)
{
    Expr pair = car(env);
    set_cdr(pair, cdr(vals));
}

Expr env_outer(Expr env)
{
    return cdr(env);
}

void env_def(Expr env, Expr var, Expr val)
{
    Expr const vals = env_find(env, var);
    if (vals)
    {
        set_car(vals, val);
    }
    else
    {
        Expr const pair = car(env);
        set_car(pair, cons(var, car(pair)));
        set_cdr(pair, cons(val, cdr(pair)));
    }
#if PROFILE_LOOKUP
    _max_vars = std::max(_max_vars, list_length(env_vars(env)));
#endif
}

void env_del(Expr env, Expr var)
{
    Expr vars = env_vars(env);
    Expr vals = env_vals(env);
    Expr prev_vars = nil;
    Expr prev_vals = nil;

    while (vars)
    {
        if (car(vars) == var)
        {
            if (prev_vars)
            {
                set_cdr(prev_vars, cdr(vars));
                set_cdr(prev_vals, cdr(vals));
            }
            else
            {
                _env_set_vars(env, cdr(vars));
                _env_set_vals(env, cdr(vals));
            }
            return;
        }
        prev_vars = vars;
        prev_vals = vals;
        vars = cdr(vars);
        vals = cdr(vals);
    }

    ERROR("cannot unbind variable %s", repr(var));
}

Bool env_owns(Expr env, Expr var)
{
    return env_find(env, var) != nil;
}

Bool env_has(Expr env, Expr var)
{
    return env_find_global(env, var) != nil;
}

void env_set(Expr env, Expr var, Expr val)
{
    Expr const vals = env_find_global(env, var);
    if (vals)
    {
        set_car(vals, val);
        return;
    }
    else
    {
        ERROR("unbound variable %s", repr(var));
    }
}

Expr env_lookup(Expr env, Expr var)
{
#if PROFILE_LOOKUP
    Profiler prof(&_lookup_time, &_lookup_call);
#endif

    Expr const iter = env_find_global(env, var);

    //if (var == QUOTE(foo))
    //{
    //    rformat("ENV-LOOKUP {} {} => {}\n", env, var, iter);
    //}

    if (iter)
    {
        return car(iter);
    }
    return ERROR("unbound variable %s", repr(var));
}

Bool env_maybe_lookup(Expr env, Expr var, Expr * val)
{
#if PROFILE_LOOKUP
    Profiler prof(&_lookup_time, &_lookup_call);
#endif

    Expr const iter = env_find_global(env, var);
    if (iter)
    {
        *val = car(iter);
        return 1;
    }

    return 0;
}

#endif

Expr env_outermost(Expr env)
{
    if (env == nil)
    {
        return env;
    }
    Expr outer = env_outer(env);
    while (outer)
    {
        env = outer;
        outer = env_outer(outer);
    }
    return env;
}

void env_destrucuring_bind(Expr env, Expr var, Expr val)
{
    //printf("DBIND %s %s\n", repr(var), repr(val));
    if (var == nil)
    {
        /* do nothing */
    }
    else if (is_cons(var)) /* TODO vectors? */
    {
        while (var)
        {
            if (is_cons(var))
            {
                ASSERT(is_cons(val));
                env_destrucuring_bind(env, car(var), car(val));
                var = cdr(var);
                val = cdr(val);
            }
            else /* last element of dotted list */
            {
                env_destrucuring_bind(env, var, val);
                break;
            }
        }
    }
    else /* assumed to be a variable */
    {
        env_def(env, var, val);
    }
}
