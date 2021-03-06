
#include "lisp.hpp"

static Bool closure_inited = 0;
static Expr function_tag  = nil;
static Expr macro_tag     = nil;

#define LEGACY 0 // TODO disabling this incurs a ~10% performance hit
#define STRUCTURE_REUSE  1

// TODO sync closure.lisp, otherwise interop may break
// (we could also try to reuse the structure of the existing lambda/syntax/defun/defmacro forms
// (lambda args stmt1 stmt2 stmt3) => ((tag . env) name args stmt1 stmt2 stmt3)
// where everything from args onwards is reused => change API and profile
// (defun name args stmt1 stmt2) => ((tag . env) name args stmt1 stmt2)
// where everything from name onwards is reused
static Expr make_closure(Expr tag, Expr env, Expr params, Expr body, Expr name)
{
#if LEGACY
    return cons(cons(tag, env), cons(params, body));
#else
    // TODO profile/reorder/optimize by access pattern?
    // TODO maybe a vector is a better idea for struct-like types?
    return cons(cons(tag, env), cons(name, cons(params, body)));
#endif
}

static Expr make_closure_from_name_and_stuff(Expr tag, Expr env, Expr name_and_stuff)
{
#if LEGACY
    return ERROR("not implemented");
#else
    //println(name_and_stuff);
    return cons(cons(tag, env), name_and_stuff);
#endif
}

static Bool is_tagged(Expr exp, Expr tag)
{
#if LEGACY
    return is_cons(exp) &&
        is_cons(car(exp)) &&
        caar(exp) == tag;
#else
    return is_cons(exp) &&
        is_cons(car(exp)) &&
        caar(exp) == tag;
#endif
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
#if LEGACY
    return cdar(closure);
#else
    return cdar(closure);
#endif
}

Expr closure_params(Expr closure)
{
#if LEGACY
    return cadr(closure);
#else
    return caddr(closure);
#endif
}

Expr closure_body(Expr closure)
{
#if LEGACY
    return cddr(closure);
#else
    return cdddr(closure);
#endif
}

Expr closure_name(Expr closure)
{
#if LEGACY
    return nil; // we don't store names!
#else
    return cadr(closure);
#endif
}

static void p_closure(PrintFun rec, Expr out, Expr exp, char const * flavor)
{
    stream_put_cstring(out, "#:<lisp ");
    stream_put_cstring(out, flavor);
    Expr const name = closure_name(exp);
    if (name)
    {
        stream_put_cstring(out, " ");
        rec(&g_sys, out, name);
    }
    // dump params and body
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

Expr make_function_from_lambda(Expr env, Expr exp, Expr name)
{
    DEBUG_ASSERT(closure_inited);
#if STRUCTURE_REUSE
    return make_closure_from_name_and_stuff(function_tag, env, cons(name, cdr(exp)));
#else
    Expr const params = cadr(exp);
    Expr const body   = cddr(exp);
    return make_closure(function_tag, env, params, body, name);
#endif
}

Expr make_function_from_defun(Expr env, Expr exp)
{
    DEBUG_ASSERT(closure_inited);
#if STRUCTURE_REUSE
    return make_closure_from_name_and_stuff(function_tag, env, cdr(exp));
#else
    Expr const name   = cadr(exp);
    Expr const params = caddr(exp);
    Expr const body   = cdddr(exp);
    return make_closure(function_tag, env, params, body, name);
#endif
}

void p_function(PrintFun rec, Expr out, Expr exp)
{
    p_closure(rec, out, exp, "function");
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

Expr make_macro_from_syntax(Expr env, Expr exp, Expr name)
{
    DEBUG_ASSERT(closure_inited);
#if STRUCTURE_REUSE
    return make_closure_from_name_and_stuff(macro_tag, env, cons(name, cdr(exp)));
#else
    Expr const params = cadr(exp);
    Expr const body   = cddr(exp);
    return make_closure(macro_tag, env, params, body, name);
#endif
}

void p_macro(PrintFun rec, Expr out, Expr exp)
{
    p_closure(rec, out, exp, "macro");
}
