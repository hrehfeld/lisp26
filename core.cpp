
#include "lisp.hpp"

#if BIND_BIGNUM && !ENABLE_BIGNUM
#define BIND_BIGNUM 0
#endif

#if BIND_VECTOR && !ENABLE_VECTOR
#define BIND_VECTOR 0
#endif

#if BIND_GENSYM && !ENABLE_GENSYM
#define BIND_GENSYM 0
#endif

#if BIND_FIXNUM && !ENABLE_FIXNUM
#define BIND_FIXNUM 0
#endif

Expr read_stream = nil;
Expr print_stream = nil;

#define FUN2(sym, name) env_def(env, QUOTE(sym), make_builtin_fun(name, NULL))
#define FUN1(sym) FUN2(sym, b_##sym)

#define MAC2(sym, name) env_def(env, QUOTE(sym), make_builtin_mac(name, NULL))
#define MAC1(sym) MAC2(sym, mac_##sym)

static void env_make_special(Expr env, Expr var)
{
    Expr const special = QUOTE(*special*);
    Expr val = nil;
    env_maybe_lookup(env, special, &val);
    env_def(env, special, cons(var, val));
}

// TODO move this
static Expr b_make_macro(Expr args, Expr env, void * user)
{
    /* TODO use default env? */
    return make_macro(car(args), cadr(args), caddr(args), nil);
}

// TODO move this
static Expr b_require(Expr args, Expr env, void * user)
{
    for (Expr iter = args; iter; iter = cdr(iter))
    {
        Expr arg = car(iter);
        if (arg == QUOTE(core))
        {
            system_bind_core(env);
        }
        else  if (arg == QUOTE(sdl2))
        {
            system_bind_sdl2(env);
        }
        else
        {
            ERROR("don't know how to require %s", repr(arg));
        }
    }
    return nil;
}

static Expr b_exit(Expr args, Expr env, void * user)
{
    exit(number_value(car(args)));
    return nil;
}

#if BIND_TYPE
static Expr b_type(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    if (!unpack_all_args(args, "x", &exp))
    {
        return ERROR("illegal arguments -- TYPE");
    }
    return f_type(exp);
}
#endif

/* (when <test> . <body>) */
static Expr mac_when(Expr args, Expr env, void * user)
{
    Expr stmt = cons(SYM_progn, cdr(args));
    if (cdr(args) && !cddr(args)) // single statement
    {
        stmt = cadr(args);
    }
    return LIST3(SYM_if, car(args), stmt);
}

void system_bind_core(Expr env)
{
    if (!read_stream)
    {
        read_stream = make_file_input_stream(stdin, "<stdin>", 0);
    }

    if (!print_stream)
    {
        print_stream = make_file_output_stream(stdout, "<stdout>", 0);
    }

#if ENABLE_SPECIAL
    env_def(env, QUOTE(*special*), nil);
#endif

    env_def(env, QUOTE(*read-stream*), read_stream);
    env_make_special(env, QUOTE(*read-stream*));

    env_def(env, QUOTE(*print-stream*), print_stream);
    env_make_special(env, QUOTE(*print-stream*));

    // TODO these probably should be dynamically scoped
    FUN1(eq);

    env_def(env, SYM_t, SYM_t);

    cons_install(env);

    FUN1(equal);

    // TODO add BIND_* for this
    MAC1(when);

#if BIND_GENSYM
    FUN1(gensym);
#endif

    bind_symbol(env);

#if BIND_FIXNUM
    bind_fixnum(env);
#endif

#if BIND_BIGNUM
    bignum_install(env);
#endif

#if BIND_TYPE
    FUN1(type);
#endif

#if BIND_EVAL
    FUN1(eval);
#endif

#if BIND_APPLY
    FUN1(apply);
#endif

    bind_printer(env);

#if BIND_STREAM
    bind_stream(env);
#endif

#if BIND_ENV
    env_install(env);
#endif

#if BIND_SYSTEM
    FUN1(system);
#endif
    FUN1(exit);

    FUN1(coerce);

    string_bind(env);
    number_bind(env);

    FUN1(nreverse); // TODO can be implemented in lisp

    FUN1(escape);
    FUN2(load-file, b_load_file);
    FUN2(load-once, b_load_once);

    reader_bind(env);

#if BIND_VECTOR
    bind_vector(env);
#endif

#if ENABLE_HASH
    hash_bind(env);
#endif

    env_def(env, QUOTE(escape), make_builtin_fun(b_escape, NULL));

    meta_install(env);

    env_def(env, QUOTE(make-macro), make_builtin_fun(b_make_macro, NULL));
    env_def(env, QUOTE(require), make_builtin_fun(b_require, NULL));
}

#undef FUN2
#undef FUN1
#undef MAC2
#undef MAC1
