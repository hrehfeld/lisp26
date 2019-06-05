
#include "lisp.hpp"

#if PRINTER_BACKQUOTE && !ENABLE_BACKQUOTE
#error cannot print backquote if disabled
#endif

#if PRINTER_UNQUOTE && !ENABLE_BACKQUOTE
#error cannot print unquote if backquote disabled
#endif

#if PRINTER_BIGNUM && !ENABLE_BIGNUM
#define PRINTER_BIGNUM 0
#endif

#if PRINTER_VECTOR && !ENABLE_VECTOR
#error cannot print vector if disabled
#endif


#if 1 // TODO add a config for this

static Bool       printer_inited = 0;
static HashSetU64 printer_visited;

static void clear_marks()
{
    if (!printer_inited)
    {
        make_hash_set(&printer_visited);
        printer_inited = 1;
    }
    hash_set_clear(&printer_visited);
    //show_set(&printer_visited);
}

static Bool is_marked(Expr exp)
{
    return hash_set_has(&printer_visited, exp);
}

static void mark_expr(Expr exp)
{
    hash_set_put(&printer_visited, exp);
}

#else

static void clear_marks()
{
}

static Bool is_marked(Expr exp)
{
    return 0;
}

static void mark_expr(Expr exp)
{
}

#endif

static void print_helper(System * sys, Expr out, Expr exp)
{
    if (exp == nil)
    {
        stream_put_cstring(out, "nil");
    }

#if ENABLE_GENSYM
    else if (is_gensym(exp))
    {
        p_gensym(print_helper, out, exp);
    }
#endif

#if ENABLE_FIXNUM
    else if (is_fixnum(exp))
    {
        stream_put_i64(out, fixnum_value(exp));
    }
#endif

#if ENABLE_FLOAT
    else if (is_float(exp))
    {
        stream_put_f32(out, float_value(exp));
    }
#endif

    else if (is_symbol(exp))
    {
        stream_put_cstring(out, symbol_name(exp)); // TODO switch to utf-8/utf-32 for symbols?
    }

    else if (is_function(exp)) /* test before cons */
    {
        p_function(print_helper, out, exp);
    }

    else if (is_macro(exp)) /* test before cons */
    {
        p_macro(print_helper, out, exp);
    }

#if PRINTER_QUOTE
    else if (is_quote(exp))
    {
        stream_put_char(out, '\'');
        print_helper(sys, out, cadr(exp));
    }
#endif

#if PRINTER_BACKQUOTE
    else if (is_backquote(exp))
    {
        stream_put_char(out, '`');
        print_helper(sys, out, cadr(exp));
    }
#endif

#if PRINTER_UNQUOTE
    else if (is_unquote(exp))
    {
        stream_put_char(out, ',');
        print_helper(sys, out, cadr(exp));
    }
#endif

#if PRINTER_UNQUOTE_SPLICE
    else if (is_unquote_splicing(exp))
    {
        stream_put_char(out, ',');
        stream_put_char(out, '@');
        print_helper(sys, out, cadr(exp));
    }
#endif

    else if (is_string(exp))
    {
        p_string(print_helper, out, exp);
    }

#if PRINTER_BIGNUM
    else if (is_bignum(exp))
    {
        bignum_print(print_helper, out, exp);
    }
#endif

#if ENABLE_FLOAT
    else if (is_float(exp))
    {
        p_float(print_helper, out, exp);
    }
#endif

    else if (is_pointer(exp))
    {
        stream_put_ptr(out, pointer_value(exp));
    }

    else if (is_builtin_fun(exp))
    {
        // TODO add names if available
        stream_put_cstring(out, "#:<core function>");
    }

    else if (is_builtin_mac(exp))
    {
        // TODO add names if available
        stream_put_cstring(out, "#:<core macro>");
    }

    else if (is_stream(exp))
    {
        stream_put_cstring(out, "#:<stream>");
    }

#if PRINTER_VECTOR
    else if (is_vector(exp))
    {
        if (is_marked(exp))
        {
            stream_put_cstring(out, "...");
        }
        else
        {
            mark_expr(exp);

            stream_put_char(out, '#');
            stream_put_char(out, '(');

            U64 const len = vector_length(exp);
            for (U64 idx = 0; idx < len; ++idx)
            {
                if (idx > 0)
                {
                    stream_put_char(out, ' ');
                }
                print_helper(sys, out, vector_get(exp, idx));
            }

            stream_put_char(out, ')');
        }
    }
#endif

    /* NOTE: always test for cons last */
    else if (is_cons(exp))
    {
        if (is_marked(exp))
        {
            stream_put_cstring(out, "...");
        }
        else
        {
            mark_expr(exp);

            stream_put_char(out, '(');
            print_helper(sys, out, car(exp));

            for (Expr tmp = cdr(exp); tmp; tmp = cdr(tmp))
            {
                if (tmp == exp)
                {
                    stream_put_cstring(out, " ...");
                    break;
                }
                else if (is_cons(tmp))
                {
                    stream_put_char(out, ' ');
                    print_helper(sys, out, car(tmp));
                }
                else
                {
                    stream_put_cstring(out, " . ");
                    print_helper(sys, out, tmp);
                    break;
                }
            }

            stream_put_char(out, ')');
        }
    }

    else
    {
        ERROR("cannot print expression");
    }
}

static void println_to_stream(Expr out, Expr exp)
{
    clear_marks();
    print_helper(&g_sys, out, exp);
    stream_put_char(out, '\n');
}

void print_to_stream(Expr out, Expr exp)
{
    clear_marks();
    print_helper(&g_sys, out, exp);
}

Expr repr_as_expr(Expr exp)
{
    Expr stream = make_string_output_stream();
    print_to_stream(stream, exp);
    Expr const ret = f_stream_to_string(stream);
    stream_close(stream);
    return ret;
}

/* TODO could cause subtle bugs if the collector runs */
char const * repr(Expr exp)
{
    return string_value(repr_as_expr(exp));
}

void print(Expr exp)
{
    Expr stream = make_file_output_stream(PRINT_FILE, 0);
    clear_marks();
    print_to_stream(stream, exp);
    stream_close(stream);
}

void println(Expr exp)
{
    Expr stream = make_file_output_stream(PRINT_FILE, 0);
    clear_marks();
    println_to_stream(stream, exp);
    stream_close(stream);
}

void print_with_env(Expr exp, Expr env)
{
    Expr stream;
    if (env_maybe_lookup(env, SYM_PRINT_STREAM, &stream))
    {
        print_to_stream(stream, exp);
    }
    else
    {
        print(exp);
    }
}

void println_with_env(Expr exp, Expr env)
{
    Expr stream;
    if (env_maybe_lookup(env, SYM_PRINT_STREAM, &stream))
    {
        println_to_stream(stream, exp);
    }
    else
    {
        println(exp);
    }
}

static Expr b_repr(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    if (!unpack_all_args(args, "x", &exp))
    {
        return ERROR("illegal arguments -- REPR");
    }
    return repr_as_expr(exp);
}

static Expr f_print(Expr exp, Expr env)
{
#if ENABLE_PRINT_REDIR
    print_with_env(exp, env);
#else
    print(exp);
#endif
    return nil;
}

static Expr b_print(Expr args, Expr env, void * user)
{
    Expr ret = nil;
    for (Expr iter = args; iter; iter = cdr(iter))
    {
        if (iter != args) /* !first */
        {
            printf(" ");
        }
        ret = f_print(car(iter), env);
    }
    return ret;
}

static Expr f_println(Expr exp, Expr env)
{
#if ENABLE_PRINT_REDIR
    println_with_env(exp, env);
#else
    println(exp);
#endif
    return nil;
}

static Expr b_println(Expr args, Expr env, void * user)
{
    Expr ret = nil;
    for (Expr iter = args; iter; iter = cdr(iter))
    {
        if (iter != args) /* !first */
        {
            printf(" ");
        }

        if (cdr(iter))
        {
            /* use println for inner expressions */
            ret = f_print(car(iter), env);
        }
        else
        {
            /* use println for last expression */
            ret = f_println(car(iter), env);
        }
    }
    return ret;
}

// TODO this does not belong here

static Expr pop(Expr * exp)
{
    Expr const ret = car(*exp);
    *exp = cdr(*exp); // TODO put into core?
    return ret;
}

static void format_to_stream(Expr out, Expr fmt, Expr args) /* TODO reuse this for rformat? */
{
    char const * str = string_value(fmt);
    /* TODO scan entire string and direly output
       if not a format string (like printf => puts) */
    for (char const * pch = str; *pch; ++pch)
    {
        char ch = *pch;
        if (ch == '{')
        {
            ch = *++pch;
            if (ch == '}')
            {
                ch = *++pch;
                if (ch == '}')
                {
                    stream_put_char(out, ch);
                }
                else
                {
                    --pch;
                    if (!args)
                    {
                        ERROR("too few arguments for %s", repr(fmt));
                    }
                    Expr exp = pop(&args);
                    if (/*!repr_strings && */is_string(exp))
                    {
                        stream_put_cstring(out, string_value(exp));
                    }
                    else
                    {
                        print_to_stream(out, exp);
                    }
                }
            }
            else if (ch == '{')
            {
                stream_put_char(out, ch);
            }
            else
            {
                ERROR("unexpected '%c' after '{'", *pch);
            }
        }
        else if (ch == '}')
        {
            ch = *++pch;
            if (ch == '}')
            {
                stream_put_char(out, ch);
            }
            else
            {
                ERROR("unexpected '%c' after '}'", *pch);
            }
        }
        else
        {
            stream_put_char(out, ch);
        }
    }

    /* TODO complain if there are extra args */
}

static Expr format_with_args(Expr out, Expr fmt, Expr args)
{
    if (out == nil)
    {
        /* TODO unwind-protect stream_close */
        out = make_string_output_stream();
        format_to_stream(out, fmt, args);
        Expr ret = f_stream_to_string(out);
        stream_close(out);
        return ret;
    }
    else if (out == SYM_t)
    {
        /* TODO unwind-protect stream_close */
        /* TODO respect *print-stream* */
        out = make_file_output_stream(PRINT_FILE, 0);
        format_to_stream(out, fmt, args);
        stream_close(out);
        return nil;
    }
    else
    {
        format_to_stream(out, fmt, args);
        return nil;
    }
}

static Expr fun_format(Expr args, Expr env, void * user)
{
    Expr out = car(args);
    args = cdr(args);
    Expr fmt = car(args);
    args = cdr(args);

    return format_with_args(out, fmt, args);
}

void bind_printer(Expr env)
{
    env_def(env, QUOTE(repr   ), make_builtin_fun(b_repr   , NULL));
    env_def(env, QUOTE(print  ), make_builtin_fun(b_print  , NULL));
    env_def(env, QUOTE(println), make_builtin_fun(b_println, NULL));

    env_def(env, QUOTE(format), make_builtin_fun(fun_format, NULL));
}
