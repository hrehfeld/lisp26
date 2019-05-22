
#include "lisp.hpp"

static Expr vunpack_args(int * count, Expr args, char const * fmt, va_list ap)
{
    ASSERT(fmt);

    int ret = 0;

    char const * p = fmt;
    Expr tmp = args;
loop:
    {
        if (tmp == nil)
        {
            goto done;
        }

        if (*p == ' ')
        {
            goto loop;
        }

        if (*p == 0)
        {
            goto done;
        }

        /* TODO cons? */

        Expr val = car(tmp);
        //printf("%s ", fmt);
        //println(args);

        switch (*p)
        {
        case 'x':
            *va_arg(ap, Expr *) = val;
            break;
        case 's':
            *va_arg(ap, char const **) = string_value(val);
            break;
        case 'p':
            *va_arg(ap, void **) = pointer_value(val);
            break;
        case 'i':
            *va_arg(ap, int *) = num_to_int(val);
            break;
        case 'u':
            if (p[1] == 3)
            {
                *va_arg(ap, U32 *) = num_to_u32(val);
                p += 2;
                break;
            }
            else
            {
                ERROR("illegal argument char '%c' for unsigned", *p);
                goto done;
            }
//        case 'z':
//            *va_arg(ap, Size *) = as_size(val);
//            break;
//        case 'i':
//            *va_arg(ap, I32 *) = as_i32(val);
//            break;
//        case 'f':
//            *va_arg(ap, F32 *) = as_f32(val);
//            break;
//        case 'F':
//            *va_arg(ap, F64 *) = as_f64(val);
//            break;
        default:
            ERROR("illegal argument char '%c'", *p);
            goto done;
        }

        tmp = cdr(tmp);
        ++ret;
    }
    goto loop;

done:
    *count = ret;
    return tmp;
}

static int count_args(char const * fmt)
{
    ASSERT(fmt);
    int ret = 0;
    for (char const * p = fmt; *p; ++p)
    {
        if (*p == ' ')
        {
            continue;
        }
        ++ret;
    }
    return ret;
}

int unpack_args(Expr args, char const * fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int ret = 0;
    vunpack_args(&ret, args, fmt, ap);
    va_end(ap);
    return ret;
}

Bool unpack_all_args(Expr args, char const * fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int ret = 0;
    Expr iter = vunpack_args(&ret, args, fmt, ap);
    va_end(ap);
    return ret == count_args(fmt) && !iter;
}

Expr b_eq(Expr args, Expr env, void * user)
{
    Expr a = nil;
    Expr b = nil;
    if (!unpack_all_args(args, "xx", &a, &b))
    {
        return ERROR("illegal arguments -- EQ");
    }
    return f_eq(a, b);
}

Expr b_equal(Expr args, Expr env, void * user)
{
    Expr a = nil;
    Expr b = nil;
    if (!unpack_all_args(args, "xx", &a, &b))
    {
        return ERROR("illegal arguments -- EQ");
    }
    return f_equal(a, b);
}

Expr b_eval(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    if (unpack_args(args, "xx", &exp, &env) < 1)
    {
        return ERROR("illegal arguments -- EVAL");
    }
    return f_eval(exp, env);
}

Expr b_apply(Expr args, Expr env, void * user)
{
    Expr op = nil;
    Expr args1 = nil;
    if (unpack_args(args, "xxx", &op, &args1, &env) < 2)
    {
        return ERROR("illegal arguments -- APPLY");
    }
    return f_apply(op, args1, env);
}

Expr b_escape(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    if (!unpack_all_args(args, "x", &exp))
    {
        return ERROR("illegal arguments");
    }
    return f_escape(exp);
}

#if ENABLE_GENSYM

Expr b_gensym(Expr args, Expr env, void * user)
{
    if (!unpack_all_args(args, ""))
    {
        return ERROR("illegal arguments");
    }

    return f_gensym(&g_sys.gensym);
}

#endif

Expr b_load_file(Expr args, Expr env, void * user)
{
    return f_load_file(car(args), env);
}

Expr b_load_once(Expr args, Expr env, void * user)
{
    return f_load_once(car(args), env);
}

Expr b_nreverse(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    if (!unpack_all_args(args, "x", &exp))
    {
        return ERROR("illegal arguments -- NREVERSE");
    }
    return nreverse(exp);
}

Expr b_system(Expr args, Expr env, void * user)
{
    Expr exp = car(args);
    if (is_string(exp))
    {
        // TODO
        // (defun system args
        //   (std:system (join " " args)))
        return make_number(system(string_value(exp)));
    }
    else
    {
        // TODO " ".join(map(str, args))
        return ERROR("illegal arguments -- SYSTEM");
    }
}

Expr b_coerce(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    Expr type = nil;
    if (!unpack_all_args(args, "xx", &exp, &type))
    {
        return ERROR("illegal arguments -- COERCE");
    }
    return f_coerce(exp, type);
}
