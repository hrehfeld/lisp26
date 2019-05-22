
#pragma once

void cb_init(ConsBuffer * cb);
void cb_free(ConsBuffer * cb);
void cb_gc(ConsBuffer * cb, U64 num_roots, Expr ** roots);

Expr cb_cons(ConsBuffer * cb, Expr a, Expr b);
Expr * cb_car(ConsBuffer * cb, Expr exp);
Expr * cb_cdr(ConsBuffer * cb, Expr exp);
