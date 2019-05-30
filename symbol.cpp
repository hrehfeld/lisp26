
#include "lisp.hpp"
#include "gc.hpp"

static Bool   symbol_inited  = 0;

// TODO move these into symbol struct
#if PROFILE_INTERN
F64 _intern_time = 0.0;
U64 _intern_call = 0;
#endif

static void do_realloc(SymbolState * symbol)
{
    symbol->names_w = (char **) realloc(symbol->names_w, sizeof(char *) * symbol->max);
    if (!symbol->names_w)
    {
        ERROR("symbol memory allocation failed");
    }
    symbol->names_r = symbol->names_w;
}

static void maybe_realloc(SymbolState * symbol)
{
    if (symbol->num < symbol->max)
    {
        return;
    }

    if (MAX_SYMBOLS == -1 || symbol->max * 2 <= MAX_SYMBOLS)
    {
        if (symbol->max == 0)
        {
            symbol->max = DEF_SYMBOLS;
        }
        else
        {
            symbol->max *= 2;
        }
        do_realloc(symbol);
        return;
    }

    ERROR("intern ran over memory budget");
}

#if SYMBOL_CACHE

static void intern_cached(SymbolState * symbol, char const * name, Expr exp)
{
    Expr const sym = intern(name);
    if (sym != exp)
    {
        fprintf(stderr, "symbol cache mismatch %s => 0x%" PRIx64 " != 0x%" PRIx64 "\n",
                name, expr_bits(sym), expr_bits(exp));
    }
    ++symbol->ncached;
}

static Bool _symbol_complain = 0;

void cache_symbols(SymbolState * symbol)
{
    // can only cache at beginning of buffer
    ASSERT(symbol->num == 0);

    symbol->ncached = 0;

    intern_cached(symbol, "."               , SYM_DOT);
    intern_cached(symbol, "#:error"         , SYM_ERROR);
    intern_cached(symbol, "*print-stream*"  , SYM_PRINT_STREAM);

    intern_cached(symbol, "*env*"           , SYM__env_);
    intern_cached(symbol, "*special*"       , SYM__special_);

    intern_cached(symbol, "assert"          , SYM_assert);
    intern_cached(symbol, "backquote"       , SYM_backquote);
    intern_cached(symbol, "block"           , SYM_block);
    intern_cached(symbol, "catch"           , SYM_catch);
    intern_cached(symbol, "cons"            , SYM_cons);
    intern_cached(symbol, "defun"           , SYM_defun);
    intern_cached(symbol, "env-bind"        , SYM_env_bind);
    intern_cached(symbol, "env-set"         , SYM_env_set);
    intern_cached(symbol, "env-unbind"      , SYM_env_unbind);
    intern_cached(symbol, "fixnum"          , SYM_fixnum);
    intern_cached(symbol, "go"              , SYM_go);
    intern_cached(symbol, "if"              , SYM_if);
    intern_cached(symbol, "lambda"          , SYM_lambda);
    intern_cached(symbol, "let"             , SYM_let);
    intern_cached(symbol, "progn"           , SYM_progn);
    intern_cached(symbol, "quote"           , SYM_quote);
    intern_cached(symbol, "require"         , SYM_require);
    intern_cached(symbol, "return-from"     , SYM_return_from);
    intern_cached(symbol, "string"          , SYM_string);
    intern_cached(symbol, "symbol"          , SYM_symbol);
    intern_cached(symbol, "syntax"          , SYM_syntax);
    intern_cached(symbol, "t"               , SYM_t);
    intern_cached(symbol, "tagbody"         , SYM_tagbody);
    intern_cached(symbol, "throw"           , SYM_throw);
    intern_cached(symbol, "unquote"         , SYM_unquote);
    intern_cached(symbol, "unquote-splicing", SYM_unquote_splicing);
    intern_cached(symbol, "unwind-protect"  , SYM_unwind_protect);
    intern_cached(symbol, "while"           , SYM_while);
    intern_cached(symbol, "macroexpand-1"   , SYM_macroexpand_1);
    intern_cached(symbol, "label"           , SYM_label);
    _symbol_complain = 1;
}

#endif

void symbol_init(SymbolState * symbol)
{
    memset(symbol, 0, sizeof(SymbolState));

    maybe_realloc(symbol);

    symbol_inited = 1;
#if SYMBOL_CACHE
    // NOTE call is after inited check
    cache_symbols(symbol);
#endif
}

void symbol_quit(SymbolState * symbol)
{
    symbol->names_r = NULL;
    free(symbol->names_w);
    symbol->names_w = NULL;
    symbol_inited = 0;
}

static void walk(Expr * pexp, HashSetU64 * visited)
{
    printf("%s\n", __FUNCTION__);

    ASSERT(pexp);
    Expr const exp = *pexp;

    if (is_symbol(exp))
    {
        *pexp = intern(symbol_name(exp));
    }

    else if (is_cons(exp))
    {
        printf("%s:%d\n", __FILE__, __LINE__);
        printf("cons %s\n", repr(exp));

        U64 const bits = expr_bits(exp);
        if (hash_set_has(visited, bits))
        {
            //printf("skipping %s\n", repr(exp));
            return;
        }

        hash_set_put(visited, bits);

        walk(gc_car(exp), visited);
        walk(gc_cdr(exp), visited);
    }

#if ENABLE_VECTOR
    else if (is_vector(exp))
    {
        printf("%s:%d\n", __FILE__, __LINE__);
        printf("vector %s\n", repr(exp));

        U64 const bits = expr_bits(exp);
        if (hash_set_has(visited, bits))
        {
            printf("skipping %s\n", repr(exp));
            return;
        }

        hash_set_put(visited, bits);

        U64 len = vector_length(exp);
        for (U64 idx = 0; idx < len; ++idx)
        {
            walk(gc_vector_get(exp, idx), visited);
        }
    }
#endif

    else
    {
        printf("%s:%d\n", __FILE__, __LINE__);
        printf("unknown %s\n", repr(exp));
    }
}

void symbol_gc(SymbolState * symbol, U64 num_roots, Expr ** roots)
{
    return;

    ASSERT(symbol_inited);
//#if TRACE_COLLECT
    U64 num = symbol->num;
//#endif

    printf("%s\n", __FUNCTION__);

    // create a new write buffer
    symbol->num     = 0;
    symbol->names_w = (char **) malloc(sizeof(char *) * symbol->max);
    ASSERT(symbol->names_w);
#if SYMBOL_CACHE
    // TODO we should be able to just copy them
    cache_symbols(symbol);
#endif

    HashSetU64 visited;
    make_hash_set(&visited);
    printf("%s\n", __FUNCTION__);

    for (U64 root_id = 0; root_id < num_roots; ++root_id)
    {
        Expr * root = roots[root_id];
        ASSERT(root);
        walk(root, &visited);
    }
    printf("%s\n", __FUNCTION__);

    free_hash_set(&visited);

    printf("%s\n", __FUNCTION__);

    for (U64 i = 0; i < num; ++i)
    {
        free(symbol->names_r[i]);
    }
    free(symbol->names_r);
    symbol->names_r = symbol->names_w;

#if TRACE_COLLECT
    printf("SYMBOL GC: %" PRIu64 " => %" PRIu64 "\n", num, symbol->num);
#endif
}

Bool is_symbol(Expr exp)
{
	//TODO: needs ifdef for different nil configs for max performance?
    return exp == nil || expr_type(exp) == TYPE_SYMBOL;
}

static Expr _make_symbol(SymbolState * symbol, char const * name, size_t len)
{
#if PROFILE_INTERN
    Profiler prof(&_intern_time, &_intern_call);
#endif
    ASSERT(symbol_inited);

    if (len == 3 && !strncmp("nil", name, 3))
    {
        return nil;
    }

    for (U64 index = 0; index < symbol->num; ++index)
    {
        char const * str = symbol->names_w[index];
        size_t const tmp = strlen(str); // TODO cache this?
        if (len == tmp && !strncmp(name, str, len))
        {
            return make_expr(TYPE_SYMBOL, index);
        }
    }

    maybe_realloc(symbol);
    U64    const index  = symbol->num;
    char       * buffer = (char *) malloc(len + 1);
    memcpy(buffer, name, len);
    buffer[len] = 0;
    symbol->names_w[index] = buffer;
    ++symbol->num;
    return make_expr(TYPE_SYMBOL, index);
}

void symbol_gc(U64 num_roots, Expr ** roots)
{
    symbol_gc(&g_sys.symbol, num_roots, roots);
}

Expr make_symbol(char const * name)
{
    return _make_symbol(&g_sys.symbol, name, strlen(name));
}

Expr intern_range(char const * begin, char const * end)
{
    return _make_symbol(&g_sys.symbol, begin, end - begin);
}

Expr make_symbol_ex(char const * name, char const * file, int line)
{
#if SYMBOL_CACHE && SYMBOL_CACHE_COMPLAIN
    if (_symbol_complain)
    {
        fprintf(stderr, "%s:%d INTERN %s\n", file, line, name);
    }
#endif

    return make_symbol(name);
}

char const * symbol_name(Expr exp)
{
    ASSERT(symbol_inited);

    if (exp == nil)
    {
        return "nil";
    }

    if (!is_symbol(exp))
    {
        ERROR("cannot get symbol name of expression");
    }

    U64 const index = expr_data(exp);
    if (index >= (&g_sys.symbol)->num)
    {
        ERROR("illegal symbol index %" PRIu64, index);
    }
    return (&g_sys.symbol)->names_r[index];
}

#if BIND_SYMBOL

static Expr fun_intern(Expr args, Expr env, void * user)
{
    char const * str = NULL;
    if (!unpack_all_args(args, "s", &str))
    {
        return ERROR("illegal arguments -- INTERN");
    }
    return intern(str);
}

static Expr fun_symbol_name(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    if (!unpack_all_args(args, "x", &exp))
    {
        return ERROR("illegal arguments -- SYMBOL-NAME");
    }
    return f_symbol_name(exp);
}

#endif

void bind_symbol(Expr env)
{
#if BIND_SYMBOL
    env_def(env, QUOTE(intern)     , make_builtin_fun(fun_intern, NULL));
    env_def(env, QUOTE(symbol-name), make_builtin_fun(fun_symbol_name, NULL));
#endif
}
