
#ifndef _RUNTIME_HPP_
#define _RUNTIME_HPP_

#include "lisp.hpp"

#endif

#ifdef LISP_IMPLEMENTATION

#ifndef _RUNTIME_CPP_
#define _RUNTIME_CPP_

#include "bignum.cpp"
#include "builtin.cpp"
#include "builtin_misc.cpp"
#include "closure.cpp"
#include "coerce.cpp"
#include "cons.cpp"
#include "cons_bind.cpp"
#include "cons_impl.cpp"
#include "core.cpp"
#include "env.cpp"
#include "env_install.cpp"
#include "env_test.cpp"
#include "error.cpp"
#include "eval.cpp"
#include "expr.cpp"
#include "fixnum.cpp"
#include "float.cpp"
#include "gensym.cpp"
#include "hash.cpp"
#include "hash_bind.cpp"
#include "hash_impl.cpp"
#include "list.cpp"
#include "meta.cpp"
#include "number.cpp"
#include "pointer.cpp"
#include "printer.cpp"
#include "reader.cpp"
#include "reader_bind.cpp"
#include "reader_test.cpp"
#include "sdl2.cpp"
#include "spooky.cpp"
#include "stream.cpp"
#include "stream_impl.cpp"
#include "string.cpp"
#include "symbol.cpp"
#include "system.cpp"
#include "test.cpp"
#include "time.cpp"
#include "util.cpp"
#include "vector.cpp"

#endif

#endif
