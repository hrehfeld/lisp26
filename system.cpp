
#include "lisp.hpp"

/**************/

void load_file(char const * ifn, Expr env)
{
    //fprintf(stderr, "loading %s...\n", ifn);
    ErrorContext ec("load-file %s", ifn);

    Expr in = make_file_input_stream_from_path(ifn);

    Expr exp = nil;
    while (maybe_read_from_stream(in, &exp))
    {
        eval(exp, env);
    }

    stream_close(in);
}

Expr read_file(char const * ifn) // TODO move to reader.cpp
{
    Expr in = make_file_input_stream_from_path(ifn);
    Expr ret = read_all_from_stream(in);
    stream_close(in);
    return ret;
}

/**************/

System g_sys;

void system_init()
{
#if ENABLE_GENSYM
    gensym_init(&g_sys.gensym);
#endif
    symbol_init(&g_sys.symbol);
    cons_init();
    string_init();
#if ENABLE_BIGNUM
    bignum_init();
#endif
#if ENABLE_VECTOR
    vector_init();
#endif
#if ENABLE_HASH
    hash_init();
#endif
    closure_init();
    eval_init();
}

void system_quit()
{
    eval_quit();
    closure_quit();
#if ENABLE_HASH
    hash_quit();
#endif
#if ENABLE_VECTOR
    vector_quit();
#endif
#if ENABLE_BIGNUM
    bignum_quit();
#endif
    string_quit();
    cons_quit();
    symbol_quit(&g_sys.symbol);
#if ENABLE_GENSYM
    gensym_quit();
#endif
}

static void system_gc(Expr * env)
{
    // TODO profile which order is fastest
    cons_gc(1, &env);
#if ENABLE_VECTOR
    vector_gc(1, &env);
#endif
    symbol_gc(1, &env);
    string_gc(1, &env);
#if ENABLE_BIGNUM
    bignum_gc();
#endif
}

void system_repl(Expr env)
{
     // TODO make a proper prompt input stream
    Expr in = make_file_input_stream(stdin, "<stdin>", 0);
    Expr exp = nil;
    Expr ret = nil;

loop:
    try
    {
        /* read */
        fprintf(PRINT_FILE, "> ");
        fflush(PRINT_FILE);

        Bool const have = maybe_read_from_stream(in, &exp);
        if (!have)
        {
            goto done;
        }

        /* eval */
        ret = f_eval(exp, env);

        /* print */
        println(ret);
    }
    catch (Expr err)
    {
        fprintf(stderr, "REPL error: %s\n", repr(err));
    }

    system_gc(&env); // TODO this does not modify the input env!

    goto loop;

done:
    stream_close(in);
}

void system_eval(char const * src, Expr env)
{
    Expr exp = nil;
    exp = read_from_string(src);
    f_eval(exp, env);
}

void system_load(char const * ifn, Expr env)
{
    load_file(ifn, env);
}
