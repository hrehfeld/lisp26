
#include "lisp.hpp"
#include "cons_impl.hpp"

#define DEF_CONSES 4

void cb_init(ConsBuffer * cb)
{
    cb->num   = 0;
    cb->max   = 0;
    cb->pairs = NULL;
}

void cb_free(ConsBuffer * cb)
{
    cb->num = 0;
    cb->max = 0;
    free(cb->pairs);
}

void cb_gc(ConsBuffer * cb, U64 num_roots, Expr ** roots)
{
#if TRACE_COLLECT
    U64 num = cb->num;
#endif

    Pair * pairs = (Pair *) malloc(sizeof(Pair) * cb->max);

    Pair * psrc = cb->pairs;
    Pair * pdst = pairs;

    Expr const broken_heart = make_expr(TYPE_NIL, DATA_BROKEN_HEART);

    U64 ifree = 0;

     /* forward cons roots to new buffer */
    for (U64 root_id = 0; root_id < num_roots; ++root_id)
    {
        Expr * root = roots[root_id];
        ASSERT(root);
        if (is_cons(*root))
        {
            U64 isrc = expr_data(*root);
            pdst[ifree]   = psrc[isrc];
            psrc[isrc ].a = broken_heart; /* mark as moved */
            psrc[isrc ].b = bits_expr(ifree);
            ++ifree;
        }
    }

    U64 scan = 0;
    while (scan < ifree)
    {
        Expr * ptr;

        ptr = &pdst[scan].a;
        if (is_cons(*ptr))
        {
            U64  isrc = expr_data(*ptr);
            Expr tag  = psrc[isrc].a;
            U64  idst = expr_bits(psrc[isrc].b);

            if (tag == broken_heart)
            {
                *ptr = make_expr(TYPE_CONS, idst);
            }
            else
            {
                pdst[ifree] = psrc[isrc];
                psrc[isrc].a = broken_heart;
                psrc[isrc].b = bits_expr(ifree);
                *ptr = make_expr(TYPE_CONS, ifree);
                ++ifree;
            }
        }

        ptr = &pdst[scan].b;
        if (is_cons(*ptr))
        {
            U32  isrc = expr_data(*ptr);
            Expr tag  = psrc[isrc].a;
            U32  idst = expr_bits(psrc[isrc].b);

            if (tag == broken_heart)
            {
                *ptr = make_expr(TYPE_CONS, idst);
            }
            else
            {
                pdst[ifree] = psrc[isrc];
                psrc[isrc].a = broken_heart;
                psrc[isrc].b = bits_expr(ifree);
                *ptr = make_expr(TYPE_CONS, ifree);
                ++ifree;
            }
        }
        ++scan;
    }

    for (U64 root_id = 0; root_id < num_roots; ++root_id)
    {
        Expr * root = roots[root_id];
        ASSERT(root);
        if (is_cons(*root))
        {
            U64 const isrc = expr_data(*root);
            U64 const idst = expr_bits(psrc[isrc].b);
            *root = make_expr(TYPE_CONS, idst);
        }
    }

    cb->num = ifree;
    cb->pairs = pdst;
    pairs = psrc;

    free(pairs);

#if TRACE_COLLECT
    printf("CONS GC: %" PRIu64 " => %" PRIu64 "\n", num, cb->num);
#endif
}

static void cb_realloc(ConsBuffer * cb)
{
    cb->pairs = (Pair *) realloc(cb->pairs, sizeof(Pair) * cb->max);
    if (!cb->pairs)
    {
        ERROR("cons memory allocation failed");
    }
}

static void cb_maybe_realloc(ConsBuffer * cb)
{
    if (cb->num < cb->max)
    {
        /* still enough space */
        return;
    }

    if (MAX_CONSES == -1 || cb->max * 2 <= MAX_CONSES)
    {
        if (cb->max == 0)
        {
            cb->max = DEF_CONSES;
        }
        else
        {
            cb->max *= 2;
        }

        cb_realloc(cb);
        return;
    }

    ERROR("cons ran over memory budget");
}

static Pair * cb_lookup(ConsBuffer * cb, U64 index)
{
    DEBUG_ASSERT(index < cb->num);
    return &cb->pairs[index];
}

Expr cb_cons(ConsBuffer * cb, Expr a, Expr b)
{
    cb_maybe_realloc(cb);

    U64 const index = cb->num++;
    Pair * cons = cb_lookup(cb, index);
    cons->a = a;
    cons->b = b;
    return make_expr(TYPE_CONS, index);
}

Expr * cb_car(ConsBuffer * cb, Expr exp)
{
    ASSERT(is_cons(exp));

    U64 const index = expr_data(exp);
    Pair * cons = cb_lookup(cb, index);
    return &cons->a;
}

Expr * cb_cdr(ConsBuffer * cb, Expr exp)
{
    ASSERT(is_cons(exp));

    U64 const index = expr_data(exp);
    Pair * cons = cb_lookup(cb, index);
    return &cons->b;
}
