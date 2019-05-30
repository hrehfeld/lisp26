
#ifndef DEBUG
#define DEBUG                   1 // 1 0 1
#endif

#define NIL_IS_MACRO            1 // 1 0 1
#define CXR_NIL_IS_NIL          1 // 1 0 1

#define MAX_SYMBOLS            -1
#define MAX_CONSES             -1
#define MAX_STRINGS          1024
#define MAX_POINTERS         1024
#define MAX_BUILTINS         1024
#define MAX_STREAMS           256 // TODO there's a leak somewhere if set too small

#define DEF_SYMBOLS            16

#define MAX_STACK_FRAMES      100

#define ENABLE_GENSYM           1 // 1 0 1
#define ENABLE_FIXNUM           1 // 1 0 1
#define ENABLE_BIGNUM           0 // 0 0 1
#define ENABLE_FLOAT            1 // 1 0 1
#define ENABLE_VECTOR           1 // 1 0 1
#define ENABLE_BACKQUOTE        1 // 1 0 1

#define ENABLE_ARGS             1 // 1 0 1
#define ENABLE_WHILE            1 // 1 0 1
#define ENABLE_REST             1 // 1 0 1
#define ENABLE_SPECIAL          1 // 1 0 1
#define ENABLE_PRINT_REDIR      1 // 1 0 1
#define ENABLE_BACKTRACE        1 // 1 0 1
#define ENABLE_HASH             1 // 1 0 1

#define ENABLE_STATUS           1 // 1 0 1

#define ENABLE_SMART_EXPR       0 // 0 0 1

#ifndef ENABLE_SDL2
#define ENABLE_SDL2             1 // 1 0 1
#endif

#define CHECK_DIV0              1 // 1 0 1
#define CHECK_OVERFLOWS         1 // 1 0 1
#define SYMBOL_CACHE            1 // 1 0 1
#define SYMBOL_CACHE_COMPLAIN   0 // 0 0 1
#define VECTOR_BOUNDS_CHECK     1 // 1 0 1

// TODO defun is broken due to missing block?
#define EVAL_BLOCK              1 // 1 0 1
#define EVAL_CATCH              1 // 1 0 1
#define EVAL_DEFUN              0 // 0 0 1
#define EVAL_ENV                1 // 1 0 1
#define EVAL_LET                1 // 1 0 1
#define EVAL_PROGN              1 // 1 0 1
#define EVAL_TAGBODY            1 // 1 0 1
#define EVAL_UNWIND_PROTECT     1 // 1 0 1

#define EVAL_OPT_TAIL_CALLS     1 // 1 0 1
#define EVAL_FIXNUM_VARS        1 // 1 0 1
#define EVAL_STACK_MARKER       1 // 0 0 1

// TODO rename these to CORE_BIND_*
#define BIND_EVAL               1 // 1 0 1
#define BIND_APPLY              1 // 1 0 1
#define BIND_GENSYM             1 // 1 0 1
#define BIND_FIXNUM             1 // 1 0 1
#define BIND_BIGNUM             0 // 0 0 1
#define BIND_VECTOR             1 // 1 0 1
#define BIND_STREAM             1 // 1 0 1
#define BIND_ENV                1 // 1 0 1
#define BIND_SYMBOL             1 // 1 0 1
#define BIND_SYSTEM             1 // 1 0 1
#define BIND_HASH               1 // 1 0 1
#define BIND_TYPE               0 // 0 0 1

#define READER_QUOTE            1 // 1 0 1
#define READER_BACKQUOTE        1 // 1 0 1
#define READER_FIXNUM           1 // 1 0 1
#define READER_VECTOR           1 // 1 0 1
#define READER_USE_NUMBER       1 // 1 0 1
#define READER_TILDE_IS_UNQUOTE 1 // 1 0 1
#define READER_SPLIT_ATTR       1 // 1 0 1

#define PRINTER_QUOTE           1 // 1 0 1
#define PRINTER_BACKQUOTE       ENABLE_BACKQUOTE
#define PRINTER_UNQUOTE         ENABLE_BACKQUOTE
#define PRINTER_VECTOR          ENABLE_VECTOR
#define PRINTER_BIGNUM          0 // 0 0 1

#define TRACE_EVAL              0 // 0 0 1
#define TRACE_LOCK              0 // 0 0 1
#define TRACE_PROTECT           0 // 0 0 1
#define TRACE_COLLECT           0 // 0 0 1
#define TRACE_READ              0 // 0 0 1
#define TRACE_STREAM            0 // 0 0 1
#define TRACE_BACKQUOTE         0 // 0 0 1

#define PROFILE_INTERN          0 // 0 0 1
#define PROFILE_LOOKUP          0 // 0 0 1
#define PROFILE_GC              0 // 0 0 1

#define EXIT_ON_ERROR           0 // 0 0 1
#define PRINT_ON_ERROR          0 // 0 0 1
#define INLINE_EXPR_FUNS        1 // 1 0 1
#define FORCE_FULL_IF           0 // 0 0 1
#define SYSTEM_ASSERT           0 // 0 0 1
#define IS_NIL_FUNCTION         0 // 0 0 1

#define READ_FILE               stdin
#define PRINT_FILE              stdout
#define ERROR_FILE              stderr

#define TEST_CATCH_ERRORS       1 // 1 0 1
#define TEST_STOP_ON_FAIL       0 // 0 0 1

// TODO still crashes
#define ENV_USE_HASH            0 // 0 0 1

#if ENABLE_HASH && !ENABLE_VECTOR
#undef ENABLE_VECTOR
#define ENABLE_VECTOR 1
#endif

#if ENABLE_HASH && !ENABLE_FIXNUM
#undef ENABLE_FIXNUM
#define ENABLE_FIXNUM 1
#endif
