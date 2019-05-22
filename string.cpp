
#include "lisp.hpp"

struct StringBuffer
{
    U64    count;
    char * values[MAX_STRINGS];
};

StringBuffer _sb;

static Expr string_alloc(size_t len)
{
    if (_sb.count < MAX_STRINGS)
    {
        U64 const index = _sb.count;
        _sb.values[index] = (char *) malloc(len + 1);
        ++_sb.count;
        return make_expr(TYPE_STRING, index);
    }

    return ERROR("cannot make string of length %d", (int) len);
}

static char * string_buffer(Expr exp)
{
    ASSERT(is_string(exp));

    U64 const index = expr_data(exp);
    if (index >= _sb.count)
    {
        ERROR("illegal string index %" PRIu64, index);
    }

    return _sb.values[index];
}

void string_init()
{
}

void string_quit()
{
}

Bool is_string(Expr exp)
{
    return expr_type(exp) == TYPE_STRING;
}

Expr make_string(char const * str)
{
    size_t const len = strlen(str);

    /* TODO fixstrs sound attractive,
       but the string_value API breaks
       b/c it has to return a pointer */

    Expr ret = string_alloc(len);
    memcpy(string_buffer(ret), str, len + 1);
    return ret;
}

char const * string_value(Expr exp)
{
    return string_buffer(exp);
}

size_t string_length(Expr exp)
{
    /* TODO cache this */
    return strlen(string_value(exp));
}

Expr string_add(Expr a, Expr b)
{
    char const * x = string_value(a);
    char const * y = string_value(b);
    size_t const lx = strlen(x);
    size_t const ly = strlen(y);
    Expr ret = string_alloc(lx + ly);
    char * buf = string_buffer(ret);
    memcpy(buf, x, lx + 1);
    memcpy(buf + lx, y, ly + 1);
    return ret;
}

static Expr _string_mul(Expr a, Expr b)
{
    char const * x = string_value(a);
    // TODO should we cache this? -> at least put it in a function
    size_t const lx = strlen(x);
    size_t const vb = num_to_size(b);
    Expr ret = string_alloc(lx * vb + 1);
    char * buf = string_buffer(ret);
    for (size_t i = 0; i < vb; ++i)
    {
        memcpy(buf, x, lx + 1);
        buf += lx;
    }
    return ret;
}

Expr string_mul(Expr a, Expr b)
{
    if (is_string(a))
    {
        return _string_mul(a, b);
    }
    else if (is_string(b))
    {
        return _string_mul(b, a);
    }
    else
    {
        return ERROR("Cannot multiply %s and %s", repr(a), repr(b));
    }
}

void string_gc(U64 num_roots, Expr ** roots)
{
#if TRACE_COLLECT
    U64 num_strings = _sb.count;
#endif

    /* do nothing */

#if TRACE_COLLECT
    printf("STRING GC: %" PRIu64 " => %" PRIu64 "\n", num_strings, _sb.count);
#endif
}

void p_string(PrintFun rec, Expr out, Expr exp)
{
    stream_put_char(out, '"');
    char const * str = string_value(exp);
    size_t const   len = string_length(exp);
    for (size_t i = 0; i < len; ++i)
    {
        char const ch = str[i];
        switch (ch)
        {
        case '"':
            stream_put_char(out, '\\');
            stream_put_char(out, '"');
            break;
        case '\n':
            stream_put_char(out, '\\');
            stream_put_char(out, 'n');
            break;
        case '\t':
            stream_put_char(out, '\\');
            stream_put_char(out, 't');
            break;
        default:
            stream_put_char(out, ch);
            break;
        }
    }
    stream_put_char(out, '"');
}

static Expr b_string_add(Expr args, Expr env, void * user)
{
    /* TODO special case for two args? */
    Expr ret = make_string("");
    for (Expr iter = args; iter; iter = cdr(iter))
    {
        Expr arg = car(iter);
        ret = string_add(ret, arg); // TODO rename to string_add2 and use foldl
    }
    return ret;
}

static Expr b_string_mul(Expr args, Expr env, void * user)
{
    Expr exp1 = nil;
    Expr exp2 = nil;
    if (!unpack_all_args(args, "xx", &exp1, &exp2))
    {
        return ERROR("illegal arguments -- STRING-MUL");
    }
    return string_mul(exp1, exp2);
}

static Expr b_string_len(Expr args, Expr env, void * user)
{
    return size_to_num(string_length(car(args)));
}

static Expr b_string_char_at(Expr args, Expr env, void * user)
{
    return u32_to_num(string_value(car(args))[num_to_u64(cadr(args))]);
}

void string_bind(Expr env)
{
    // TODO rename to string:*
    env_def(env, QUOTE(string-add)    , make_builtin_fun(b_string_add, NULL));
    env_def(env, QUOTE(string-mul)    , make_builtin_fun(b_string_mul, NULL));
    env_def(env, QUOTE(string-len)    , make_builtin_fun(b_string_len, NULL));
    env_def(env, QUOTE(string-char-at), make_builtin_fun(b_string_char_at, NULL));
}
