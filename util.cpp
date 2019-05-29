
#include "lisp.hpp"

Expr make_truth(Bool b)
{
    if (b)
    {
        return SYM_t;
    }
    return nil;
}

/* TODO this is unfinished */
/* TODO we could implement this in std.lisp */
Bool equal(Expr a, Expr b)
{
    //rformat("'{}' vs. '{}'\n", a, b);
    //printf("%016" PRIx64 " vs. %016" PRIx64 "\n", a, b);

    if (eq(a, b))
    {
        return 1;
    }

    if (is_string(a) && is_string(b))
    {
        // TODO early out for different lengths
        return !strcmp(string_value(a), string_value(b));
    }

    if (is_cons(a) && is_cons(b))
    {
        // TODO check for self-references
        return
            equal(car(a), car(b)) &&
            equal(cdr(a), cdr(b));
    }

#if ENABLE_VECTOR
    if (is_vector(a) && is_vector(b))
    {
        U64 const N = vector_length(a);
        if (N != vector_length(b))
        {
            return 0;
        }

        for (U64 i = 0; i < N; ++i)
        {
            if (!equal(vector_get(a, i), vector_get(b, i)))
            {
                return 0;
            }
        }

        return 1;
    }
#endif

    return 0;
}

Expr f_eq(Expr a, Expr b)
{
    return make_truth(eq(a, b));
}

Expr f_equal(Expr a, Expr b)
{
    return make_truth(equal(a, b));
}

Expr f_symbol_name(Expr exp)
{
    return make_string(symbol_name(exp));
}

Expr f_type(Expr exp)
{
    /* have to test cons-like things first */
    if (is_function(exp))
    {
        return QUOTE(function);
    }

    if (is_macro(exp))
    {
        return QUOTE(macro);
    }

    U64 const type = expr_type(exp);
    switch (type)
    {
    case TYPE_NIL:
        return nil;
#if ENABLE_GENSYM
    case TYPE_GENSYM:
        return QUOTE(gensym);
#endif
#if ENABLE_FIXNUM
    case TYPE_FIXNUM:
        return SYM_fixnum;
#endif
    case TYPE_SYMBOL:
        return SYM_symbol;
    case TYPE_CONS:
        return SYM_cons;
    case TYPE_STRING:
        return SYM_string;
    case TYPE_POINTER:
        return QUOTE(pointer);
    case TYPE_FIXPTR:
        return QUOTE(fixptr);
#if ENABLE_VECTOR
    case TYPE_VECTOR:
        return QUOTE(vector);
#endif
    case TYPE_BUILTIN_FUN:
        return QUOTE(builtin-fun);
    case TYPE_BUILTIN_MAC:
        return QUOTE(builtin-mac);
    case TYPE_STREAM:
        return QUOTE(stream);
    default:
        return ERROR("cannot handle type %" PRIu64, type);
    }
}

Expr f_load_file(Expr ifn, Expr env)
{
    load_file(string_value(ifn), env);
    return nil;
}

Expr f_load_once(Expr ifn, Expr env)
{
    Expr const sym = QUOTE(*loaded-files*);

    Expr loaded;
    if (!env_maybe_lookup(env, sym, &loaded))
    {
        loaded = make_env(nil);
        env_def(env, sym, loaded);
    }

    char const * str = string_value(ifn);
    Expr tmp = intern(str);
    if (!env_has(loaded, tmp))
    {
        load_file(str, env);
        env_def(loaded, tmp, nil);
    }

    return nil;
}

Expr f_read_file(Expr ifn)
{
    return read_file(string_value(ifn));
}

Expr f_escape(Expr str)
{
    Expr ss = make_string_output_stream();

    stream_put_char(ss, '"');
    for (char const * p = string_value(str); *p; ++p)
    {
        char ch = *p;
        if (ch == '"')
        {
            stream_put_char(ss, '\\');
            stream_put_char(ss, '"');
        }
        else if (ch == '\n')
        {
            stream_put_char(ss, '\\');
            stream_put_char(ss, 'n');
        }
        else if (ch == '\t')
        {
            stream_put_char(ss, '\\');
            stream_put_char(ss, 't');
        }
        else if (ch == '\\') // TODO \xnn
        {
            stream_put_char(ss, '\\');
            stream_put_char(ss, '\\');
        }
        else
        {
            stream_put_char(ss, ch);
        }
    }
    stream_put_char(ss, '"');

    Expr ret = f_stream_to_string(ss);
    stream_close(ss);
    return ret;
}

// TODO move this

static void vfformat(Bool repr_strings, FILE * file, char const * fmt, va_list ap)
{
    ASSERT(fmt);
    for (char const * pch = fmt; *pch; ++pch)
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
                    fputc(ch, file);
                }
                else
                {
                    --pch;
                    Expr exp = va_arg(ap, Expr);
                    if (!repr_strings && is_string(exp))
                    {
                        fputs(string_value(exp), file);
                    }
                    else
                    {
                        fputs(repr(exp), file); // TODO don't need to repr, pass in the stream
                    }
                }
            }
            else if (ch == '{')
            {
                fputc(ch, file);
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
                fputc(ch, file);
            }
            else
            {
                ERROR("unexpected '%c' after '}'", *pch);
            }
        }
        else
        {
            fputc(ch, file);
        }
    }
}

void rformat(char const * fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfformat(1, stdout, fmt, ap);
    va_end(ap);
}

