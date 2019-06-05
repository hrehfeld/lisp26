
#include "lisp.hpp"

#if EVAL_FIXNUM_VARS && !ENABLE_FIXNUM
#define EVAL_FIXNUM_VARS 0
#endif

#if EVAL_STACK_MARKER
Expr _eval_stack = nil;
/* TODO GC

 1) stuff in the _eval_stack is not added to roots!
 2) whatever is reachable only from the eval stack bloats memory (low importance)

 */
#endif

#if EVAL_STACK_MARKER
struct StackMarker
{
    StackMarker(Expr exp)
    {
        _eval_stack = cons(exp, _eval_stack);
    }

    ~StackMarker()
    {
        _eval_stack = cdr(_eval_stack);
    }
};
#endif

class Evaluator
{
public:
    Evaluator()
    {
    }

#if TRACE_EVAL
    Expr _eval(Expr exp, Expr env)
#else
    Expr eval(Expr exp, Expr env)
#endif
    {
        //StackMarker sm(exp);

        //ErrorContext ec("eval %s", repr(exp));
        //ErrorContext ec("eval");

#if EVAL_OPT_TAIL_CALLS
    dispatch:
#endif

#if EVAL_FIXNUM_VARS
        if (is_fixnum(exp) && env_has(env, exp))
        {
            return env_lookup(env, exp);
        }
        else
#endif

            if (is_self_eval(exp))
            {
                return exp;
            }

#if 1 // TODO add a flag for this?
            else if (exp == SYM__env_)
            {
                return env;
            }
#endif

            else if (is_variable(exp))
            {
                return env_lookup(env, exp);
            }

            else if (is_cons(exp))
            {
                Expr const op = car(exp);
                Expr const args = cdr(exp);

                if (is_builtin_fun(op))
                {
#if EVAL_STACK_MARKER
                    StackMarker sm(op);
#endif
                    return apply(op, eval_list(args, env), env);
                }

                else if (is_function(op))
                {
#if EVAL_STACK_MARKER
                    StackMarker sm(op);
#endif
                    //ErrorContext ec("apply %s", repr(exp));

                    Expr vals = eval_list(args, env);
#if EVAL_OPT_TAIL_CALLS
                    //Expr vars = closure_params(op);
                    Expr body = closure_body(op);
                    exp = body;
                    env = make_call_env(op, vals, env);
                    goto eval_body;
#else
                    return apply(op, vals, env);
#endif
                }

                else if (is_builtin_mac(op))
                {
#if EVAL_STACK_MARKER
                    StackMarker sm(op);
#endif
                    return eval(apply(op, args, env), env);
                }

                else if (is_macro(op))
                {
#if EVAL_STACK_MARKER
                    StackMarker sm(op);
#endif
                    // TODO tail call optimization?
                    return eval(apply(op, args, env), env);
                }

                /* TODO do some kind of f-expr lookup for below items, unless we need to tail-call optimize them? */

                /* TODO ?
                   else if (is_variable(op))
                   {
                   return ...
                   }*/

                else if (is_quote(exp))
                {
                    return cadr(exp);
                }

#if ENABLE_BACKQUOTE
                else if (is_backquote(exp))
                {
                    return eval_backquote(exp, env);
                }
#endif

                /* (if <test> <then> <else>) */
                else if (is_if(exp))
                {
#if EVAL_OPT_TAIL_CALLS
                    if (eval(cadr(exp), env))
                    {
                        exp = caddr(exp);
                        goto dispatch;
                    }
                    else if (cdddr(exp))
                    {
                        exp = cadddr(exp);
                        goto dispatch;
                    }
                    else
                    {
                        return nil;
                    }
#else
                    return eval_if(exp, env);
#endif
                }

                /* TODO ...see the pattern yet? */

#if ENABLE_WHILE
                else if (is_while(exp))
                {
                    return eval_while(exp, env);
                }
#endif

                else if (is_op(exp, SYM_label))
                {
#if BYPASS_LABEL
                    return eval(caddr(exp), env);
#else
                    // TODO this is a bit drastic
                    Expr const name = cadr(exp);
                    Expr const sub = caddr(exp);
                    if (is_lambda(sub))
                    {
                        return eval_lambda(sub, env, name);
                    }
                    else if (is_syntax(sub))
                    {
                        return eval_syntax(sub, env, name);
                    }
                    else
                    {
                        return ERROR("label expressions may only be used with lambda or syntax");
                    }
#endif
                }

                else if (is_lambda(exp))
                {
                    return eval_lambda(exp, env, nil);
                }

                else if (is_syntax(exp))
                {
                    return eval_syntax(exp, env, nil);
                }

#if EVAL_ENV
                else if (is_env_bind(exp))
                {
                    return eval_env_bind(exp, env);
                }
                else if (is_env_unbind(exp))
                {
                    return eval_env_unbind(exp, env);
                }
                else if (is_env_set(exp))
                {
                    return eval_env_set(exp, env);
                }
#endif

                else if (is_assert(exp))
                {
                    return eval_assert(exp, env);
                }

#if EVAL_CATCH
                else if (is_catch(exp))
                {
                    return eval_catch(exp, env);
                }

                else if (is_throw(exp))
                {
                    return eval_throw(exp, env);
                }
#endif

#if EVAL_BLOCK
                else if (is_block(exp))
                {
                    return eval_block(exp, env);
                }

                else if (is_return_from(exp))
                {
                    return eval_return_from(exp, env);
                }
#endif

#if EVAL_TAGBODY
                else if (is_tagbody(exp))
                {
                    return eval_tagbody(exp, env);
                }

                else if (is_go(exp))
                {
                    return eval_go(exp, env);
                }
#endif

#if EVAL_UNWIND_PROTECT
                else if (is_unwind_protect(exp))
                {
                    return eval_unwind_protect(exp, env);
                }
#endif

#if EVAL_REQUIRE
                else if (is_require(exp))
                {
                    return eval_require(exp, env);
                }
#endif

#if EVAL_LET
                /* (let <decls> . <body>) */
                else if (op == SYM_let)
                {
#if EVAL_OPT_TAIL_CALLS
                    //printf("HARD-CODED LET...\n");
                    Expr decls = cadr(exp);
                    Expr body  = cddr(exp);

                    Expr local_env = make_env(env);

                    for (Expr iter = decls; iter; iter = cdr(iter))
                    {
                        Expr decl = car(iter);
                        Expr var = car(decl);
                        Expr val = eval(cadr(decl), env);
                        env_destrucuring_bind(local_env, var, val);
                    }

                    exp = body;
                    env = local_env;
                    goto eval_body;
#else
                    return eval_let(exp, env);
#endif
                }
#endif

#if EVAL_DEFUN
                /* (defun <name> <args> . <body>) */
                else if (op == SYM_defun)
                {
                    return eval_defun(exp, env);
                }
#endif

#if EVAL_PROGN
                /* (progn <body> ) */
                else if (op == SYM_progn)
                {
#if EVAL_OPT_TAIL_CALLS
                    exp = cdr(exp);
                    goto eval_body;
#else
                    return eval_body(cdr(exp), env);
#endif
                }
#endif

                else if (op == SYM_macroexpand_1)
                {
                    Expr form = eval(cadr(exp), env);
                    if (is_cons(form))
                    {
                        Expr name = car(form);
                        Expr macro;
                        if (env_maybe_lookup(env, name, &macro))
                        {
                            return apply(macro, cdr(form), env);
                        }
                    }
                    return form;
                }

                {
                    //ErrorContext ec("call %s", repr(exp));

                    Expr const new_op = eval(op, env);
                    if (is_error(new_op))
                    {
                        return new_op;
                    }
                    else if (new_op == op)
                    {
                        return ERROR("cannot eval self-evaluating operator %s\n", repr(op));
                    }
                    else
                    {
#if EVAL_OPT_TAIL_CALLS
                        exp = cons(new_op, cdr(exp));
                        goto dispatch;
#else
                        return eval(cons(new_op, cdr(exp)), env);
#endif
                    }
                }
            }

            else
            {
                return ERROR("cannot evaluate expression: %s", repr(exp));
            }

#if EVAL_OPT_TAIL_CALLS
    eval_body:
        {
            Expr ret = nil;
            for (Expr iter = exp; iter; iter = cdr(iter))
            {
                Expr stmt = car(iter);
                /* last statement */
                if (!cdr(iter))
                {
                    exp = stmt;
                    goto dispatch;
                }
                ret = eval(stmt, env);
            }
            return ret;
        }
#endif
    }

#if TRACE_EVAL
    Expr eval(Expr exp, Expr env)
    {
        Expr ret = nil;
        //printf("EVAL "); println(exp);
        rformat("EVAL {}\n", exp);
        ret = _eval(exp, env);
        //printf("EVAL "); print(exp); printf(" => "); println(ret);
        rformat("EVAL {} => {}\n", exp, ret);
        return ret;
    }
#endif

    Expr apply(Expr op, Expr vals, Expr env)
    {
        if (is_closure(op))
        {
            return eval_body(closure_body(op), make_call_env(op, vals, env));
        }
        else if (is_builtin_fun(op))
        {
            return builtin_apply(op)(vals, env, builtin_user(op));
        }
        else if (is_builtin_mac(op))
        {
            return builtin_apply(op)(vals, env, builtin_user(op));
        }
        else
        {
#if EVAL_STACK_MARKER
            return ERROR("cannot apply %s %s", repr(op), repr(_eval_stack));
#else
            return ERROR("cannot apply %s", repr(op));
#endif
        }
    }

protected:
    Expr eval_body(Expr body, Expr env)
    {
        Expr ret = nil;
        for (Expr iter = body; iter; iter = cdr(iter))
        {
            ret = eval(car(iter), env);
        }
        return ret;
    }

    Expr eval_list(Expr exps, Expr env)
    {
        Expr ret = nil;
        for (Expr iter = exps; iter; iter = cdr(iter))
        {
            ret = cons(eval(car(iter), env), ret);
        }
        return nreverse(ret);
    }

    Bool is_self_eval(Expr exp)
    {
        if (exp == nil)
        {
            return 1;
        }

#if ENABLE_FIXNUM
        if (is_fixnum(exp))
        {
            return 1;
        }
#endif

#if ENABLE_FLOAT
        if (is_float(exp))
        {
            return 1;
        }
#endif

        if (is_string(exp))
        {
            return 1;
        }

#if ENABLE_VECTOR
        if (is_vector(exp))
        {
            return 1;
        }
#endif

        if (is_stream(exp))
        {
            return 1;
        }

        return 0;
    }

    Bool is_variable(Expr exp)
    {
        if (is_symbol(exp))
        {
            return 1;
        }

#if ENABLE_GENSYM
        if (is_gensym(exp))
        {
            return 1;
        }
#endif

        return 0;
    }

    /* TODO API is somewhat ad-hoc */
    Expr make_call_env(Expr op, Expr vals, Expr env)
    {
        Expr fenv = closure_env(op);
        Expr denv = fenv; /* dynamic env */

#if ENABLE_SPECIAL
        Expr special;
        if (env_maybe_lookup(env, SYM__special_, &special))
        {
            denv = make_env(denv);
            for (Expr iter = special; iter; iter = cdr(iter))
            {
                Expr var = car(iter);
                env_def(denv, var, env_lookup(env, var));
            }
        }
#endif

        Expr cenv = make_env(denv);
        Expr vars = closure_params(op);

        // TODO catch errors when not using ENABLE_ARGS/ENABLE_REST
        env_destrucuring_bind(cenv, vars, vals);

        return cenv;
    }

    /* while **********/

#if ENABLE_WHILE

    inline Bool is_while(Expr exp)
    {
        return is_op(exp, SYM_while);
    }

    Expr eval_while(Expr exp, Expr env)
    {
        Expr const test = cadr(exp);
        Expr const body = cddr(exp);
        Expr ret = nil;
        while (eval(test, env))
        {
            ret = eval_body(body, env);
        }
        return ret;
    }

#endif

    /* tagbody/go ********/

#if EVAL_TAGBODY

    inline Bool is_go(Expr exp)
    {
        return is_op(exp, SYM_go);
    }

    Expr eval_go(Expr exp, Expr env)
    {
        throw LIST2(SYM_go, cadr(exp));
        return nil;
    }

    inline Bool is_tagbody(Expr exp)
    {
        return is_op(exp, SYM_tagbody);
    }

    Expr eval_tagbody(Expr exp, Expr env)
    {
        Expr result = nil;
        Expr const body = cdr(exp);
        for (Expr iter = body; iter; iter = cdr(iter))
        {
            Expr stmt = car(iter);

            if (is_variable(stmt))
            {
                /* skip labels */
                continue;
            }

            /* TODO opt: check if statment is (go <label>) and avoid throw/rethrow */

            try
            {
                result = eval(stmt, env);
            }
            catch (Expr err)
            {
                //printf("caught %s\n", repr(err));
                if (is_go(err))
                {
                    Expr const label = cadr(err);
                    /* find label in body */
                    for (iter = body; iter; iter = cdr(iter))
                    {
                        if (eq(car(iter), label))
                        {
                            break;
                        }
                    }

                    /* label found */
                    if (iter)
                    {
                        continue; /* resume iteration through body */
                    }

                    throw err;
                }
                else
                {
                    throw err;
                }
            }
        }

        return result;
    }

#endif

#if ENABLE_BACKQUOTE

    /* awfully recursive, hacky implementation */
    Expr backquote_list(Expr seq, Expr env)
    {
#if TRACE_BACKQUOTE
        printf("BACKQUOTE-LIST %s\n", repr(seq));
#endif

        if (seq)
        {
            Expr item = car(seq);
            Expr rest = cdr(seq);
            if (is_unquote_splicing(item))
            {
                return append(eval(cadr(item), env), backquote_list(rest, env));
            }
            else
            {
                return cons(backquote(item, env), backquote_list(rest, env));
            }
        }
        else
        {
            return nil;
        }
    }

    Expr backquote(Expr exp, Expr env)
    {
    #if TRACE_BACKQUOTE
        printf("BACKQUOTE %s\n", repr(exp));
    #endif

        if (is_cons(exp))
        {
            if (is_unquote(exp))
            {
                return eval(cadr(exp), env);
            }
            else
            {
                return backquote_list(exp, env);
            }
        }
        else
        {
            return exp;
        }
    }

    Expr eval_backquote(Expr exp, Expr env)
    {
#if TRACE_BACKQUOTE
        printf("EVAL-BACKQUOTE %s\n", repr(exp));
#endif
        return backquote(cadr(exp), env);
    }

#endif

/* if *************/

    inline Bool is_if(Expr exp)
    {
        return is_op(exp, SYM_if);
    }

#if !EVAL_OPT_TAIL_CALLS

    Expr eval_if(Expr exp, Expr env) /* TODO cache c*r calls? (test this) */
    {
        if (eval(cadr(exp), env))
        {
            return eval(caddr(exp), env);
        }
#if !FORCE_FULL_IF
        else if (cdddr(exp))
#endif
        {
            return eval(cadddr(exp), env);
        }
#if !FORCE_FULL_IF
        else
        {
            return nil;
        }
#endif
    }

#endif

/* lambda *********/

    inline Bool is_lambda(Expr exp)
    {
        return is_op(exp, SYM_lambda);
    }

    Expr eval_lambda(Expr exp, Expr env, Expr name)
    {
        return make_function_from_lambda(env, exp, name);
    }

/* syntax *********/

    inline Bool is_syntax(Expr exp)
    {
        return is_op(exp, SYM_syntax);
    }

    Expr eval_syntax(Expr exp, Expr env, Expr name)
    {
        return make_macro_from_syntax(env, exp, name);
    }

/* env-* *********/

#if EVAL_ENV

    inline Bool is_env_bind(Expr exp)
    {
        return
            is_op(exp, SYM_env_bind) ||
            is_op(exp, QUOTE(env-def)); // TODO
    }

    Expr eval_env_bind(Expr exp, Expr env)
    {
        Expr const env_ = eval(cadr(exp), env);
        Expr const var_ = eval(caddr(exp), env);
        Expr const val_ = eval(cadddr(exp), env);
        env_def(env_, var_, val_);
        return nil;
    }

    inline Bool is_env_unbind(Expr exp)
    {
        return
            is_op(exp, SYM_env_unbind) ||
            is_op(exp, QUOTE(env-del));
    }

    Expr eval_env_unbind(Expr exp, Expr env)
    {
        Expr const env_ = eval(cadr(exp), env);
        Expr const var_ = eval(caddr(exp), env);
        env_del(env_, var_);
        return nil;
    }

    inline Bool is_env_set(Expr exp)
    {
        return is_op(exp, SYM_env_set);
    }

    Expr eval_env_set(Expr exp, Expr env)
    {
        Expr const env_ = eval(cadr(exp), env);
        Expr const var_ = eval(caddr(exp), env);
        Expr const val_ = eval(cadddr(exp), env);
        env_set(env_, var_, val_);
        return nil;
    }

#endif

/* assert *********/

    inline Bool is_assert(Expr exp)
    {
        return is_op(exp, SYM_assert);
    }

    Expr eval_assert(Expr exp, Expr env)
    {
        Expr const arg = cadr(exp);
        if (eval(arg, env))
        {
            return nil;
        }
        else
        {
            return ERROR("assertion failed: %s", repr(arg));
        }
    }

/* catch/throw ********/

#if EVAL_CATCH

    inline Bool is_throw(Expr exp)
    {
        return is_op(exp, SYM_throw);
    }

    Expr eval_throw(Expr exp, Expr env)
    {
        Expr const tag    = eval(cadr(exp), env);
        Expr const result = eval(caddr(exp), env);
        throw LIST3(SYM_throw, tag, result);
        return nil;
    }

    inline Bool is_catch(Expr exp)
    {
        return is_op(exp, SYM_catch);
    }

    inline Expr eval_catch(Expr exp, Expr env)
    {
        Expr const tag  = eval(cadr(exp), env);
        Expr const forms = cddr(exp);
        Expr ret = nil;
        for (Expr iter = forms; iter; iter = cdr(iter))
        {
            Expr stmt = car(iter);
            try
            {
                ret = eval(stmt, env);
            }
            catch (Expr err)
            {
                //println(err);
                if (is_throw(err))
                {
                    Expr const name = cadr(err);
                    if (eq(name, tag))
                    {
                        return caddr(err);
                    }
                }

                throw err;
            }
        }
        return ret;
    }

#endif

/* block/return-from ********/

#if EVAL_BLOCK

    inline Bool is_return_from(Expr exp)
    {
        return is_op(exp, SYM_return_from);
    }

    Expr eval_return_from(Expr exp, Expr env)
    {
        Expr const name = cadr(exp);
        if (cddr(exp))
        {
            Expr const result = eval(caddr(exp), env);
            throw LIST3(SYM_return_from, name, result);
        }
        else
        {
            throw LIST2(SYM_return_from, name);
        }
        return nil;
    }

    inline Bool is_block(Expr exp)
    {
        return is_op(exp, SYM_block);
    }

    inline Expr eval_block(Expr exp, Expr env)
    {
        Expr const block_name = cadr(exp);
        Expr const body = cddr(exp);
        Expr ret = nil;
        for (Expr iter = body; iter; iter = cdr(iter))
        {
            Expr stmt = car(iter);
            try
            {
                ret = eval(stmt, env);
            }
            catch (Expr err)
            {
                //println(err);
                if (is_return_from(err))
                {
                    Expr const name = cadr(err);
                    if (eq(name, block_name))
                    {
                        if (cddr(err))
                        {
                            return caddr(err);
                        }
                        else
                        {
                            return nil;
                        }
                    }
                }

                throw err;
            }
        }
        return ret;
    }

#endif

/* unwind-protect */

//
// (unwind-protect <stmt> <body>...)
//

#if EVAL_UNWIND_PROTECT

    inline Bool is_unwind_protect(Expr exp)
    {
        return is_op(exp, SYM_unwind_protect);
    }

    Expr eval_unwind_protect(Expr exp, Expr env)
    {
        Expr const stmt = cadr(exp);
        Expr const body = cddr(exp);
        Expr ret = nil;
        try
        {
            ret = eval(stmt, env);
        }
        catch (...)
        {
            for (Expr iter = body; iter; iter = cdr(iter))
            {
                ret = eval(car(iter), env);
            }

            throw;
        }

        for (Expr iter = body; iter; iter = cdr(iter))
        {
            ret = eval(car(iter), env);
        }
        return ret;
    }

#endif

#if EVAL_LET && !EVAL_OPT_TAIL_CALLS

/* (let <decls> . <body>) */

    Expr eval_let(Expr exp, Expr env)
    {
        Expr decls = cadr(exp);
        Expr body  = cddr(exp);

        Expr local_env = make_env(env);

        for (Expr iter = decls; iter; iter = cdr(iter))
        {
            Expr decl = car(iter);
            Expr var = car(decl);
            Expr val = eval(cadr(decl), env);
            env_destrucuring_bind(local_env, var, val);
        }

        return eval_body(body, local_env);
    }

#endif

#if EVAL_DEFUN

/* (defun <name> <args> . <body>) */

    Expr eval_defun(Expr exp, Expr env)
    {
        env_def(env, name, make_function_from_defun(env, exp));
        return nil;
    }

#endif
};

void eval_init()
{
}

void eval_quit()
{
}

Expr eval(Expr exp, Expr env)
{
    Evaluator evaluator;
    return evaluator.eval(exp, env);
}

Expr apply(Expr op, Expr vals, Expr env)
{
    Evaluator evaluator;
    return evaluator.apply(op, vals, env);
}

Expr eval_string(char const * src, Expr env)
{
    return eval(read_from_string(src), env);
}
