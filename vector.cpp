
#include "lisp.hpp"

#if ENABLE_VECTOR

typedef struct
{
    U64    num;
    U64    max;
    Expr * data; // TODO keep/test separate length array
} VectorBuffer;

static void vb_init(VectorBuffer * vb)
{
    memset(vb, 0, sizeof(VectorBuffer));
}

static void vb_free(VectorBuffer * vb)
{
    free(vb->data);
    memset(vb, 0, sizeof(VectorBuffer));
}

static VectorBuffer g_vb = { 0, 0, NULL };

static void vb_do_realloc(VectorBuffer * vb)
{
    //printf("%s:%d: %s()\n", __FILE__, __LINE__, __FUNCTION__);
    vb->data = (Expr *) realloc(vb->data, sizeof(Expr) * vb->max);
    ASSERT(vb->data);
}

static void vb_maybe_realloc(VectorBuffer * vb, U64 dlt)
{
    //printf("%s:%d: %s()\n", __FILE__, __LINE__, __FUNCTION__);
    Bool changed = 0;
    if (vb->max == 0)
    {
        vb->max = 1;
        changed = 1;
    }

    while (vb->num + dlt + 1 > vb->max)
    {
        vb->max *= 2;
        changed = 1;
    }

    if (changed)
    {
        vb_do_realloc(vb);
    }
}

VectorBuffer * vb = &g_vb; /* TODO remove this and pass as argument */

void vector_init()
{
    vb_init(&g_vb);
}

void vector_quit()
{
    vb_free(&g_vb);
}

void vector_gc(U64 num_roots, Expr ** roots)
{
#if TRACE_COLLECT
    U64 num = vb->num; // TODO this is not number of vectors, but number of exprs held
#endif

#if TRACE_COLLECT
    printf("VECTOR GC: %" PRIu64 " => %" PRIu64 "\n", num, vb->num);
#endif
}

Bool is_vector(Expr exp)
{
    return expr_type(exp) == TYPE_VECTOR;
}

Expr make_vector(U64 len)
{
    //printf("%s:%d: %s()\n", __FILE__, __LINE__, __FUNCTION__);
    vb_maybe_realloc(&g_vb, len);
    //printf("%s:%d: %s()\n", __FILE__, __LINE__, __FUNCTION__);

    U64 const index = vb->num;
    vb->num += len + 1;
    Expr * exprs = vb->data + index;
    *exprs++ = u64_to_num(len);

    //printf("%s:%d: %s()\n", __FILE__, __LINE__, __FUNCTION__);
    ASSERT(nil == 0);
    memset(exprs, 0, sizeof(Expr) * len);
    //printf("%s:%d: %s()\n", __FILE__, __LINE__, __FUNCTION__);
    return make_expr(TYPE_VECTOR, index);
}

Expr vector_get(Expr exp, U64 idx)
{
    ASSERT(is_vector(exp));

#if VECTOR_BOUNDS_CHECK
    ASSERT(idx < vector_length(exp));
#endif

    Expr const * base = vb->data + expr_data(exp) + 1;
    return base[idx];
}

void vector_set(Expr exp, U64 idx, Expr val)
{
    ASSERT(is_vector(exp));

#if VECTOR_BOUNDS_CHECK
    ASSERT(idx < vector_length(exp));
#endif

    Expr * base = vb->data + expr_data(exp) + 1;
    base[idx] = val;
}

U64 vector_length(Expr exp)
{
    return num_to_u64(f_vector_length(exp));
}

Expr f_vector_get(Expr exp, Expr idx)
{
    return vector_get(exp, num_to_u64(idx));
}

void f_vector_set(Expr exp, Expr idx, Expr val)
{
    vector_set(exp, num_to_u64(idx), val);
}

Expr f_vector_length(Expr exp)
{
    ASSERT(is_vector(exp));
    // TODO storing length in a fixnum is probably a stupid idea
    return vb->data[expr_data(exp)];
}

Expr * gc_vector_get(Expr exp, U64 idx)
{
    ASSERT(is_vector(exp));

#if VECTOR_BOUNDS_CHECK
    ASSERT(idx < vector_length(exp));
#endif

    Expr * base = vb->data + expr_data(exp) + 1;
    return &base[idx];
}

Expr b_make_vector(Expr args, Expr env, void * user)
{
    Expr len = nil;
    if (!unpack_all_args(args, "x", &len))
    {
        return ERROR("illegal arguments -- MAKE-VECTOR");
    }
    return make_vector(num_to_u64(len));
}

Expr b_vector_get(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    Expr idx = nil;
    if (!unpack_all_args(args, "xx", &exp, &idx))
    {
        return ERROR("illegal arguments -- VECTOR-GET");
    }
    return f_vector_get(exp, idx);
}

Expr b_vector_set(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    Expr idx = nil;
    Expr val = nil;
    if (!unpack_all_args(args, "xxx", &exp, &idx, &val))
    {
        return ERROR("illegal arguments -- VECTOR-SET");
    }
    f_vector_set(exp, idx, val);
    return nil;
}

Expr b_vector_length(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    if (!unpack_all_args(args, "x", &exp))
    {
        return ERROR("illegal arguments -- VECTOR-LENGTH");
    }
    return f_vector_length(exp);
}

void bind_vector(Expr env)
{
    env_def(env, QUOTE(make-vector  ), make_builtin_fun(b_make_vector  , NULL));
    env_def(env, QUOTE(vector-get   ), make_builtin_fun(b_vector_get   , NULL));
    env_def(env, QUOTE(vector-set   ), make_builtin_fun(b_vector_set   , NULL));
    env_def(env, QUOTE(vector-length), make_builtin_fun(b_vector_length, NULL));
}

#endif
