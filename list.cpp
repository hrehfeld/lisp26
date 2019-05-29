
#include "lisp.hpp"

Expr list()
{
    return nil;
}

Expr list(Expr exp1)
{
    return cons(exp1, nil);
}

Expr list(Expr exp1, Expr exp2)
{
    return cons(exp1, cons(exp2, nil));
}

Expr list(Expr exp1, Expr exp2, Expr exp3)
{
    return cons(exp1, cons(exp2, cons(exp3, nil)));
}

Expr list(Expr exp1, Expr exp2, Expr exp3, Expr exp4)
{
    return cons(exp1, cons(exp2, cons(exp3, cons(exp4, nil))));
}

Expr list(Expr exp1, Expr exp2, Expr exp3, Expr exp4, Expr exp5)
{
    return cons(exp1, cons(exp2, cons(exp3, cons(exp4, cons(exp5, nil)))));
}

U64 list_length(Expr exp)
{
    U64 ret = 0;
    while (exp)
    {
        ++ret;
        exp = cdr(exp);
    }
    return ret;
}

void list_append(Expr * head, Expr * tail, Expr exp)
{
    Expr const next = cons(exp, nil);
    if (*head == nil)
    {
        *head = *tail = next;
    }
    else
    {
        set_cdr(*tail, next);
        *tail = next;
    }
}

Expr append(Expr a, Expr b)
{
    return a ? cons(car(a), append(cdr(a), b)) : b;
}

Expr nreverse(Expr list)
{
    if (!list)
    {
        return list;
    }
    Expr prev = nil;
    Expr expr = list;
    while (is_cons(expr))
    {
        Expr next = cdr(expr);
        set_cdr(expr, prev);
        prev = expr;
        expr = next;
    }
    if (expr)
    {
        Expr iter;
        for (iter = prev; cdr(iter); iter = cdr(iter))
        {
            Expr next = car(iter);
            set_car(iter, expr);
            expr = next;
        }
        Expr next = car(iter);
        set_car(iter, expr);
        set_cdr(iter, next);
    }
    return prev;
}

Expr assoc(Expr item, Expr list)
{
    while (list)
    {
        Expr pair = car(list);
        if (eq(item, car(pair)))
        {
            return pair;
        }
        list = cdr(list);
    }
    return nil;
}
