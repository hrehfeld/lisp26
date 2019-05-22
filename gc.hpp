
#include "lisp.hpp"

Expr * gc_car(Expr exp);
Expr * gc_cdr(Expr exp);
Expr * gc_vector_get(Expr exp, U64 idx);
