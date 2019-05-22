
#include "lisp.hpp"

#if ENABLE_HASH && !ENABLE_VECTOR
#error hash implementation needs vectors
#endif

#if ENABLE_HASH && !ENABLE_FIXNUM
#error hash implementation needs fixnums
#endif

#if ENABLE_HASH

static Expr hash_to_expr(U64 val)
{
    //printf("%s()\n", __FUNCTION__);
    //printf("%" PRIu64 " %" PRIu64 "\n", val, DATA_MASK);
    //U64 const mask = (U64VAL(1) << (DATA_BITS - U64VAL(1))) - 1;
    //val = val & mask;
    //val = val & 0xffff;
    val = val & (DATA_MASK >> U64VAL(1));
    return make_number(val);
}

static Expr hash_string(Expr exp)
{
    /* TODO we already know the length here! */
    return hash_to_expr(hash_str(string_value(exp)));
}

static Expr hash_cons(Expr exp)
{
    return equal_hash(car(exp)) ^ equal_hash(cdr(exp));
}

static Expr hash_vector(Expr exp)
{
    U64 const N = vector_length(exp);
    Expr hash = 0;
    for (U64 i = 0; i < N; ++i)
    {
        hash = fixnum_xor(hash, equal_hash(vector_get(exp, i)));
    }
    return hash;
}

Expr eq_hash(Expr exp)
{
    return hash_to_expr(hash_u64(expr_bits(exp)));
}

/* NOTE this has to be kep in syn with Bool equal(Expr a, Expr b) */
/* TODO this breaks with cyclic structures */
Expr equal_hash(Expr exp)
{
    switch (expr_type(exp))
    {
    case TYPE_STRING:
        return hash_string(exp);

    case TYPE_CONS:
        return hash_cons(exp);

#if ENABLE_VECTOR
    case TYPE_VECTOR:
        return hash_vector(exp);
#endif

    default:
        return eq_hash(exp);
    }
}

#define HASH_DEFAULT_SIZE 31

// TODO this will get collected from under us
static Expr hash_tag = nil; // cute ;)

inline static Bool _hash_cmp(Expr a, Expr b)
{
    //return eq(a, b);
    return equal(a, b);
}

inline static Expr _hash_fun(Expr exp)
{
//    return eq_hash(exp); // TODO profile using equal instead!
    return equal_hash(exp);
}

void hash_init()
{
#if ENABLE_GENSYM
    hash_tag = make_gensym(&g_sys.gensym);
#else
    hash_tag = intern("#:<hash>");
#endif
}

void hash_quit()
{
}

Bool is_hash(Expr exp)
{
    return is_cons(exp) && eq(car(exp), hash_tag);
}

Expr make_hash()
{
    return cons(hash_tag, make_vector(HASH_DEFAULT_SIZE));
}

Expr _hash_buckets(Expr exp)
{
    DEBUG_ASSERT(is_hash(exp));
    return cdr(exp);
}

Expr _hash_index(Expr hash, Expr key)
{
    Expr const code = _hash_fun(key);
    Expr const buckets = _hash_buckets(hash);
    return number_mod(code, f_vector_length(buckets));
}

Expr _hash_find_pair(Expr hash, Expr key)
{
    Expr const index = _hash_index(hash, key);
    Expr const buckets = _hash_buckets(hash);
    for (Expr pairs = f_vector_get(buckets, index); pairs; pairs = cdr(pairs))
    {
        Expr const pair = car(pairs);
        if (_hash_cmp(key, car(pair)))
        {
            return pair;
        }
    }
    return nil;
}

Bool hash_has(Expr hash, Expr key)
{
    DEBUG_ASSERT(is_hash(hash));
    return _hash_find_pair(hash, key) != nil;
}

Bool hash_maybe_get(Expr hash, Expr key, Expr * val)
{
    DEBUG_ASSERT(is_hash(hash));
    Expr const pair = _hash_find_pair(hash, key);
    if (pair)
    {
        *val = cdr(pair);
        return 1;
    }
    return 0;
}

Expr hash_get(Expr hash, Expr key)
{
    Expr val = nil;
    if (!hash_maybe_get(hash, key, &val))
    {
        return ERROR("unbound hash key %s", repr(key));
    }
    return val;
}

void hash_put(Expr hash, Expr key, Expr val)
{
    ASSERT(is_hash(hash));

    Expr const pair = _hash_find_pair(hash, key);
    if (pair)
    {
        /* update pair */
        set_cdr(pair, val);
    }
    else
    {
        /* update bucket */
        /* TODO this recomputes the index and bucket */
        Expr const index  = _hash_index(hash, key);
        Expr const buckets = _hash_buckets(hash);
        Expr const bucket = f_vector_get(buckets, index);
        f_vector_set(buckets, index, cons(cons(key, val), bucket));
    }
}

#endif
