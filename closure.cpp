
#include "lisp.hpp"

static Bool closure_inited = 0;
static Expr function_tag  = nil;
static Expr macro_tag     = nil;

static Expr make_closure(Expr tag, Expr env, Expr params, Expr body, Expr name)
{
    return cons(cons(tag, env), cons(params, body));
}

static Bool is_tagged(Expr exp, Expr tag)
{
    return is_cons(exp) &&
        is_cons(car(exp)) &&
        caar(exp) == tag;
}

void closure_init()
{
#if ENABLE_GENSYM
    function_tag = make_gensym(&g_sys.gensym);
    macro_tag    = make_gensym(&g_sys.gensym);
#else
    function_tag = QUOTE(#:function);
    macro_tag    = QUOTE(#:macro);
#endif

    closure_inited = 1;
}

void closure_quit()
{
    closure_inited = 0;
}

Bool is_closure(Expr exp)
{
    return is_function(exp) || is_macro(exp);
}

Expr closure_env(Expr closure)
{
    return cdar(closure);
}

Expr closure_params(Expr closure)
{
    return cadr(closure);
}

Expr closure_body(Expr closure)
{
    return cddr(closure);
}

/**************/

Bool is_function(Expr exp)
{
    DEBUG_ASSERT(closure_inited);
    return is_tagged(exp, function_tag);
}

Expr make_function(Expr env, Expr params, Expr body, Expr name)
{
    DEBUG_ASSERT(closure_inited);
    return make_closure(function_tag, env, params, body, name);
}

void p_function(PrintFun rec, Expr out, Expr exp)
{
    stream_put_cstring(out, "#:<lisp function");
    if (0)
    {
        stream_put_cstring(out, " ");
        rec(&g_sys, out, closure_params(exp));
        stream_put_cstring(out, " -> ");
        rec(&g_sys, out, closure_body(exp));
    }
    stream_put_cstring(out, ">");
}

/**************/

Bool is_macro(Expr exp)
{
    DEBUG_ASSERT(closure_inited);
    return is_tagged(exp, macro_tag);
}

Expr make_macro(Expr env, Expr params, Expr body, Expr name)
{
    DEBUG_ASSERT(closure_inited);
    return make_closure(macro_tag, env, params, body, name);
}

void p_macro(PrintFun rec, Expr out, Expr exp)
{
    stream_put_cstring(out, "#:<lisp macro>");
}
