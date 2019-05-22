
#pragma once

/* config *****/

#include "config.hpp"

/* incs *******/

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <assert.h>
#include <inttypes.h>
#include <string.h>

// TODO WIN32

#if ENABLE_BACKTRACE
#include <execinfo.h>
#endif

#if ENABLE_SDL2
#include <SDL2/SDL.h>
#endif

/* defs *******/

#if SYSTEM_ASSERT
#define ASSERT(x)       assert(x)
#else
#define ASSERT(x)       do { if (!(x)) { show_backtrace(); show_error_context(); make_error(__FILE__, __LINE__, "%s:%d: assertion failed: %s", __FILE__, __LINE__, #x); } } while (0)
#endif

#if DEBUG
#define DEBUG_ASSERT(x) ASSERT(x)
#else
#define DEBUG_ASSERT(x)
#endif

#define ASSERT_EXPR(x)  do { if (!(x)) { return make_error(__FILE__, __LINE__, "%s:%d: assertion failed: %s", __FILE__, __LINE__, #x); } } while (0)
#define ASSERT_VOID(x)  do { if (!(x)) { make_error(__FILE__, __LINE__, "%s:%d: assertion failed: %s", __FILE__, __LINE__, #x); return; } } while (0)

#if NIL_IS_MACRO
#define nil             0
#endif

#define QUOTE(name)     make_symbol_ex(#name, __FILE__, __LINE__)

#define LIST0()         nil
#define LIST1(x)        cons(x, LIST0())
#define LIST2(x, y)     cons(x, LIST1(y))
#define LIST3(x, y, z)  cons(x, LIST2(y, z))

/* types ******/

typedef int      Bool; // TODO use stdbool.h?

typedef uint8_t  U8;
typedef uint16_t U16;
typedef uint32_t U32;
typedef uint64_t U64;

typedef int8_t   I8;
typedef int16_t  I16;
typedef int32_t  I32;
typedef int64_t  I64;

typedef float    F32;
typedef double   F64;

#if ENABLE_SMART_EXPR

struct Expr
{
    Expr();
    Expr(U64 bits);
    ~Expr();

    operator Bool() const;

    U64 bits;
};

#else

typedef U64 Expr;

#endif

#if !NIL_IS_MACRO
extern Expr nil;
#endif

struct System;

typedef Expr (* ApplyFun)(Expr args, Expr env, void * user);
typedef void (* PrintFun)(System * sys, Expr out, Expr exp);

typedef struct
{
    U64 * buffer;
    U64   capacity;
    U64   size;
    U64   bitset; // for "reserved" values
} HashSetU64;

/* expr *******/

/* NOTE can't use enums or const as we need U64 constants */

#define U64VAL(x) x##llu
#define I64VAL(x) x##ll

#define TYPE_BITS U64VAL(8)
#define TYPE_MASK ((U64VAL(1) << TYPE_BITS) - U64VAL(1))

#define DATA_BITS (U64VAL(64) - TYPE_BITS)
#define DATA_MASK ((U64VAL(1) << DATA_BITS) - U64VAL(1))

enum /* TODO proper ordering? */
{
    TYPE_NIL = 0,
#if ENABLE_GENSYM
    TYPE_GENSYM,
#endif
#if ENABLE_FIXNUM
    TYPE_FIXNUM,
#endif
    TYPE_SYMBOL,
    TYPE_CONS,
    TYPE_STRING,
#if ENABLE_FLOAT
    TYPE_FLOAT,
#endif
#if ENABLE_BIGNUM
    TYPE_BIGNUM,
#endif
    TYPE_POINTER,
    TYPE_FIXPTR,
#if ENABLE_VECTOR
    TYPE_VECTOR,
#endif
    TYPE_BUILTIN_FUN,
    TYPE_STREAM,
    TYPE_BUILTIN_MAC,

    /* extended types built with conses:
    TYPE_LIST,
    TYPE_FUNCTION,
    TYPE_MACRO,
    TYPE_ENV,
    */

    NUM_TYPES,
};

enum
{
    DATA_NIL = 0,
    DATA_BROKEN_HEART,
};

inline static U64 expr_bits(Expr exp)
{
#if ENABLE_SMART_EXPR
    return exp.bits;
#else
    return exp;
#endif
}

inline static Expr bits_expr(U64 bits)
{
#if ENABLE_SMART_EXPR
    Expr exp;
    exp.bits = bits;
    return exp;
#else
    return bits;
#endif
}

#define MAKE_EXPR(type, data) (((data) << TYPE_BITS) | ((type) & TYPE_MASK))

#if INLINE_EXPR_FUNS

inline static Expr make_expr(U64 type, U64 data)
{
    return bits_expr(MAKE_EXPR(type, data));
}

inline static U64 expr_type(Expr exp)
{
    return expr_bits(exp) & TYPE_MASK;
}

inline static U64 expr_data(Expr exp)
{
    return expr_bits(exp) >> TYPE_BITS;
}

#if IS_NIL_FUNCTION

inline static Bool is_nil(Expr exp)
{
    return expr_bits(exp) == nil;
}

#endif

inline static Bool eq(Expr a, Expr b)
{
    return expr_bits(a) == expr_bits(b);
}

#else

Expr make_expr(U64 type, U64 data);
U64 expr_type(Expr exp);
U64 expr_data(Expr exp);

#if IS_NIL_FUNCTION
Bool is_nil(Expr exp);
#endif

inline static Bool eq(Expr a, Expr b)
{
    return a == b;
}

#endif

/* symbol *****/

#if SYMBOL_CACHE

//#define SYM_CONST(data)      MAKE_EXPR(TYPE_SYMBOL, U64VAL(data))
#define SYM_CONST(data)      data##03

#define SYM_DOT              SYM_CONST(0x00)
#define SYM_ERROR            SYM_CONST(0x01)
#define SYM_PRINT_STREAM     SYM_CONST(0x02)

#define SYM__env_            SYM_CONST(0x03)
#define SYM__special_        SYM_CONST(0x04)

#define SYM_assert           SYM_CONST(0x05)
#define SYM_backquote        SYM_CONST(0x06)
#define SYM_block            SYM_CONST(0x07)
#define SYM_catch            SYM_CONST(0x08)
#define SYM_cons             SYM_CONST(0x09)
#define SYM_defun            SYM_CONST(0x0a)
#define SYM_env_bind         SYM_CONST(0x0b)
#define SYM_env_set          SYM_CONST(0x0c)
#define SYM_env_unbind       SYM_CONST(0x0d)
#define SYM_fixnum           SYM_CONST(0x0e)
#define SYM_go               SYM_CONST(0x0f)
#define SYM_if               SYM_CONST(0x10)
#define SYM_lambda           SYM_CONST(0x11)
#define SYM_let              SYM_CONST(0x12)
#define SYM_progn            SYM_CONST(0x13)
#define SYM_quote            SYM_CONST(0x14)
#define SYM_require          SYM_CONST(0x15)
#define SYM_return_from      SYM_CONST(0x16)
#define SYM_string           SYM_CONST(0x17)
#define SYM_symbol           SYM_CONST(0x18)
#define SYM_syntax           SYM_CONST(0x19)
#define SYM_t                SYM_CONST(0x1a)
#define SYM_tagbody          SYM_CONST(0x1b)
#define SYM_throw            SYM_CONST(0x1c)
#define SYM_unquote          SYM_CONST(0x1d)
#define SYM_unquote_splicing SYM_CONST(0x1e)
#define SYM_unwind_protect   SYM_CONST(0x1f)
#define SYM_while            SYM_CONST(0x20)
#define SYM_macroexpand_1    SYM_CONST(0x21)

#else

#define SYM_DOT              QUOTE(.)
#define SYM_ERROR            QUOTE(#:error)
#define SYM_PRINT_STREAM     QUOTE(*print-stream*)

#define SYM__env_            QUOTE(*env*)
#define SYM__special_        QUOTE(*special*)

#define SYM_assert           QUOTE(assert)
#define SYM_backquote        QUOTE(backquote)
#define SYM_block            QUOTE(block)
#define SYM_catch            QUOTE(catch)
#define SYM_cons             QUOTE(cons)
#define SYM_defun            QUOTE(defun)
#define SYM_env_bind         QUOTE(env-bind)
#define SYM_env_set          QUOTE(env-set)
#define SYM_env_unbind       QUOTE(env-unbind)
#define SYM_fixnum           QUOTE(fixnum)
#define SYM_go               QUOTE(go)
#define SYM_if               QUOTE(if)
#define SYM_lambda           QUOTE(lambda)
#define SYM_let              QUOTE(let)
#define SYM_progn            QUOTE(progn)
#define SYM_quote            QUOTE(quote)
#define SYM_require          QUOTE(require)
#define SYM_return_from      QUOTE(return-from)
#define SYM_string           QUOTE(string)
#define SYM_symbol           QUOTE(symbol)
#define SYM_syntax           QUOTE(syntax)
#define SYM_t                QUOTE(t)
#define SYM_tagbody          QUOTE(tagbody)
#define SYM_throw            QUOTE(throw)
#define SYM_unquote          QUOTE(unquote)
#define SYM_unquote_splicing QUOTE(unquote-splicing)
#define SYM_unwind_protect   QUOTE(unwind-protect)
#define SYM_while            QUOTE(while)
#define SYM_macroexpand_1    QUOTE(macroexpand-1)

#endif

#define SYM_apply            QUOTE(apply)
#define SYM_type             QUOTE(type)
#define SYM_eval             QUOTE(eval)
#define SYM_eq               QUOTE(eq)
#define SYM_car              QUOTE(car)
#define SYM_cdr              QUOTE(cdr)
#define SYM_rplaca           QUOTE(rplaca)
#define SYM_rplacd           QUOTE(rplacd)
#define SYM_equal            QUOTE(equal)
#define SYM_gensym           QUOTE(gensym)

struct Symbol
{
    U64 num;
    U64 max;
    char ** names_r;
    char ** names_w;
#if SYMBOL_CACHE
    U64 ncached;
#endif
};

void symbol_init(Symbol * symbol);
void symbol_quit(Symbol * symbol);
void symbol_gc(U64 num_roots, Expr ** roots);

Bool is_symbol(Expr exp);

Expr make_symbol(char const * name);
Expr make_symbol_ex(char const * name, char const * file, int line);

char const * symbol_name(Expr exp);

inline static Expr intern(char const * name)
{
    return make_symbol(name);
}

Expr intern_range(char const * begin, char const * end);

void bind_symbol(Expr env);

/* cons *******/

typedef struct
{
    Expr a, b;
} Pair;

typedef struct
{
    U64    num;
    U64    max;
    Pair * pairs;
} ConsBuffer;

void cons_init();
void cons_quit();
void cons_gc(U64 num_roots, Expr ** roots);
void cons_install(Expr env);

Bool is_cons(Expr exp);

Expr cons(Expr a, Expr b);
Expr car(Expr exp);
Expr cdr(Expr exp);

inline static Expr caar(Expr exp) { return car(car(exp)); }
inline static Expr cdar(Expr exp) { return cdr(car(exp)); }
inline static Expr cadr(Expr exp) { return car(cdr(exp)); }
inline static Expr cddr(Expr exp) { return cdr(cdr(exp)); }

inline static Expr caddr(Expr exp) { return car(cdr(cdr(exp))); }
inline static Expr cdddr(Expr exp) { return cdr(cdr(cdr(exp))); }

inline static Expr cadddr(Expr exp) { return car(cdr(cdr(cdr(exp)))); }

Expr make_cons(Expr a, Expr b);

void set_car(Expr exp, Expr val);
void set_cdr(Expr exp, Expr val);

Expr f_car(Expr exp);
Expr f_cdr(Expr exp);

/* gensym *******/

#if ENABLE_GENSYM

struct Gensym
{
    U64 counter;
};

void gensym_init(Gensym * gensym);
void gensym_quit();

Bool is_gensym(Expr exp);
Expr make_gensym(Gensym * gensym);

inline static Expr f_gensym(Gensym * gensym)
{
    return make_gensym(gensym);
}

void p_gensym(PrintFun rec, Expr out, Expr exp);

#endif

/* fixnum *******/

#if ENABLE_FIXNUM

#define ALL_BITS      U64VAL(0xffffffffffffffff)

/* SIGN_MASK = (1 << (DATA_BITS - 1)) */
#define SIGN_MASK     (U64VAL(1) << ((U64) DATA_BITS - U64VAL(1)))

/* BITS_MASK = 0xffffffff >> (32 - DATA_BITS + 1) */
#define BITS_MASK     (ALL_BITS >> (U64VAL(64) + U64VAL(1) - (U64) DATA_BITS))

/* FIXNUM_MINVAL = -SIGN_MASK */
#define FIXNUM_MINVAL (-(I64VAL(1) << ((I64) DATA_BITS - I64VAL(1))))

/* FIXNUM_MAXVAL = SIGN_MASK - 1 */
#define FIXNUM_MAXVAL ((I64VAL(1) << ((I64) DATA_BITS - I64VAL(1))) - I64VAL(1))

void bind_fixnum(Expr env);

Bool is_fixnum(Expr exp);
Expr make_fixnum(I64 value);
I64 fixnum_value(Expr exp);

Bool fixnum_maybe_add(Expr a, Expr b, Expr * out);
Bool fixnum_maybe_sub(Expr a, Expr b, Expr * out);
Bool fixnum_maybe_mul(Expr a, Expr b, Expr * out);

Expr fixnum_add(Expr a, Expr b);
Expr fixnum_sub(Expr a, Expr b);
Expr fixnum_mul(Expr a, Expr b);
Expr fixnum_div(Expr a, Expr b);
Expr fixnum_mod(Expr a, Expr b);
Expr fixnum_rem(Expr a, Expr b);
Expr fixnum_xor(Expr a, Expr b);

/* TODO do we need all of these? */
Bool fixnum_eq(Expr a, Expr b);
Bool fixnum_ne(Expr a, Expr b);
Bool fixnum_lt(Expr a, Expr b);
Bool fixnum_le(Expr a, Expr b);
Bool fixnum_gt(Expr a, Expr b);
Bool fixnum_ge(Expr a, Expr b);

#endif

/* bignum *******/

#if ENABLE_BIGNUM

void bignum_init();
void bignum_quit();
void bignum_gc();
void bignum_bind(Expr env);
void bignum_print(PrintFun rec, Expr out, Expr exp);

Bool is_bignum(Expr exp);
Expr make_bignum(I64 value);

Bool bignum_is_fixnum(Expr exp);
I64 bignum_value(Expr exp); // may throw

Expr fixnum_to_bignum(Expr exp);

Expr bignum_add(Expr a, Expr b);
Expr bignum_sub(Expr a, Expr b);
Expr bignum_mul(Expr a, Expr b);

void bignum_install(Expr env);

#endif

/* float *******/

#if ENABLE_FLOAT

Bool is_float(Expr exp);
Expr make_float(F32 value);

F32 float_value(Expr exp);

Expr float_div(Expr a, Expr b);

void p_float(PrintFun rec, Expr out, Expr exp);

#endif

/* number *****/

Expr make_number(I64 value);
I64 number_value(Expr exp);

Expr number_add(Expr a, Expr b);
Expr number_sub(Expr a, Expr b);
Expr number_mul(Expr a, Expr b);
Expr number_div(Expr a, Expr b); // TODO this should probably fall back to rational
Expr number_mod(Expr a, Expr b);

void number_bind(Expr env);

/* string *****/

Bool is_string(Expr exp);
Expr make_string(char const * str);

char const * string_value(Expr exp);
size_t string_length(Expr exp);

Expr string_add(Expr a, Expr b);
Expr string_mul(Expr a, Expr b);

void p_string(PrintFun rec, Expr out, Expr exp);

void string_init();
void string_quit();
void string_gc(U64 num_roots, Expr ** roots);
void string_bind(Expr env);

/* pointer ****/

Bool is_pointer(Expr exp);
Expr make_pointer(void * ptr);

void * pointer_value(Expr exp);

/* vector *****/

#if ENABLE_VECTOR

void vector_init();
void vector_quit();
void vector_gc(U64 num_roots, Expr ** roots);

Bool is_vector(Expr exp);
Expr make_vector(U64 len);

Expr vector_get(Expr exp, U64 idx);
void vector_set(Expr exp, U64 idx, Expr val);
U64 vector_length(Expr exp);

Expr f_vector_get(Expr exp, Expr idx);
void f_vector_set(Expr exp, Expr idx, Expr val);
Expr f_vector_length(Expr exp);

void bind_vector(Expr env);

#endif

/* builtin ****/

Bool is_builtin_fun(Expr exp);
Bool is_builtin_mac(Expr exp);

Expr make_builtin_fun(ApplyFun apply, void * user);
Expr make_builtin_mac(ApplyFun apply, void * user);

ApplyFun builtin_apply(Expr exp);
void * builtin_user(Expr exp);

/* builtin_misc */

Bool unpack_all_args(Expr args, char const * fmt, ...);
int unpack_args(Expr args, char const * fmt, ...);

/* all b_* functions have the same signature */

Expr b_eq(Expr args, Expr env, void * user);
Expr b_equal(Expr args, Expr env, void * user);

Expr b_eval(Expr args, Expr env, void * user);
Expr b_apply(Expr args, Expr env, void * user);

Expr b_escape(Expr args, Expr env, void * user);

#if ENABLE_GENSYM

Expr b_gensym(Expr args, Expr env, void * user);

#endif

Expr b_load_file(Expr args, Expr env, void * user);
Expr b_load_once(Expr args, Expr env, void * user);

Expr b_nreverse(Expr args, Expr env, void * user);
Expr b_system(Expr args, Expr env, void * user);
Expr b_coerce(Expr args, Expr env, void * user);

/* stream *****/

Bool is_stream(Expr exp);

Expr make_file_input_stream(FILE * file, Bool close_on_free);
Expr make_file_input_stream_from_path(char const * ifn);

Expr make_file_output_stream(FILE * file, Bool close_on_free);
Expr make_file_output_stream_from_path(char const * ifn);

Expr make_string_input_stream(char const * str);
Expr make_string_output_stream();

void stream_close(Expr exp);

char const * stream_to_string(Expr exp);
Expr f_stream_to_string(Expr exp);

void stream_put_cstring(Expr exp, char const * str);

void stream_put_char(Expr exp, U32 ch);
void stream_put_u64(Expr exp, U64 val);
void stream_put_i64(Expr exp, I64 val);
void stream_put_x64(Expr exp, U64 val);
void stream_put_ptr(Expr exp, void * ptr);
void stream_put_f32(Expr exp, F32 val);

char stream_peek_char(Expr in);
void stream_skip_char(Expr in);
char stream_get_char(Expr in);
Bool stream_at_end(Expr in);

#if ENABLE_STATUS

void stream_show_status();

#endif

void bind_stream(Expr env);

/* list *******/

Expr list();
Expr list(Expr exp1);
Expr list(Expr exp1, Expr exp2);
Expr list(Expr exp1, Expr exp2, Expr exp3);
Expr list(Expr exp1, Expr exp2, Expr exp3, Expr exp4);

U64 list_length(Expr exp);

void list_append(Expr * head, Expr * tail, Expr exp);

Expr append(Expr a, Expr b);

Expr nreverse(Expr list);
Expr assoc(Expr item, Expr list);

/* closure ****/

void closure_init();
void closure_quit();

Bool is_closure(Expr exp);

Expr closure_env(Expr closure);
Expr closure_params(Expr closure);
Expr closure_body(Expr closure);

Bool is_function(Expr exp);
Expr make_function(Expr env, Expr params, Expr body, Expr name);  /* name can be nil */

void p_function(PrintFun rec, Expr out, Expr exp);

Bool is_macro(Expr exp);
Expr make_macro(Expr env, Expr params, Expr body, Expr name);

void p_macro(PrintFun rec, Expr out, Expr exp);

/* env ********/

#define ENV_DEF_BUILTIN(env, name, fun)    env_def(env, name, make_builtin_fun(fun, NULL))

Expr make_env(Expr outer);

Expr env_vars(Expr env);

/* local */
void env_def(Expr env, Expr var, Expr val);
void env_del(Expr env, Expr var);
Bool env_owns(Expr env, Expr var);

/* global */
Bool env_has(Expr env, Expr var);
void env_set(Expr env, Expr var, Expr val);

Expr env_lookup(Expr env, Expr var);
Bool env_maybe_lookup(Expr env, Expr var, Expr * val);

inline static
void env_alias(Expr env, Expr var, Expr otr)
{
    env_def(env, var, env_lookup(env, otr));
}

inline static
Expr env_get(Expr env, Expr var)
{
    return env_lookup(env, var);
}

/*

TODO note that you can do stuff like this, so might as well return NIL

if (int const i = 42)
{
    printf("%d\n", i);
}

*/

Expr env_outer(Expr env);
Expr env_outermost(Expr env);
void env_destrucuring_bind(Expr env, Expr var, Expr val);

void env_install(Expr env);
void env_test();

/* reader *****/

Expr read_with_env(Expr env);

Expr read_from_string(char const * str);

Expr read_from_stream(Expr in);
Bool maybe_read_from_stream(Expr in, Expr * exp);
Expr read_all_from_stream(Expr in);

void reader_bind(Expr env);
void reader_test();

/* printer ****/

char const * repr(Expr exp);
void print(Expr exp);
void println(Expr exp);

void print_with_env(Expr exp, Expr env);
void println_with_env(Expr exp, Expr env);

void print_to_stream(Expr out, Expr exp);
Expr repr_as_expr(Expr exp);

void bind_printer(Expr env);

/* eval *******/

void eval_init();
void eval_quit();

Expr eval(Expr exp, Expr env);
Expr apply(Expr op, Expr vals, Expr env);

Expr eval_string(char const * src, Expr env);

void load_file(char const * ifn, Expr env);
Expr read_file(char const * ifn);

/* error ******/

#define ERROR(...)   make_error(__FILE__, __LINE__, __VA_ARGS__)

class ErrorContext
{
public:
    ErrorContext(char const * fmt, ...);
    ~ErrorContext();
};

Bool is_error(Expr exp);
Expr make_error(char const * file, int line, char const * fmt, ...);

void show_backtrace();
void show_error_context();

/* hash *******/

#if ENABLE_HASH

Expr equal_hash(Expr exp);
Expr eq_hash(Expr exp);

void hash_init();
void hash_quit();

Bool is_hash(Expr exp);
Expr make_hash();

Bool hash_maybe_get(Expr hash, Expr key, Expr * val);

Bool hash_has(Expr hash, Expr key);
Expr hash_get(Expr hash, Expr key);
void hash_put(Expr hash, Expr key, Expr val);

void hash_bind(Expr env);

#endif

/* coerce *****/

/* x_as_y -> reinterprets/truncates */
/* x_to_y -> may throw errors */

I32 u32_as_i32(U32 value);
F32 u32_as_f32(U32 value);
U32 i32_as_u32(I32 value);
F32 i32_as_f32(I32 value);
U32 f32_as_u32(F32 value);
I32 f32_as_i32(F32 value);

I64 u64_as_i64(U64 value);
F64 u64_as_f64(U64 value);
U64 i64_as_u64(I64 value);
F64 i64_as_f64(I64 value);
U64 f64_as_u64(F64 value);
I64 f64_as_i64(F64 value);

Expr int_to_num(int val);
int  num_to_int(Expr val);

Expr u32_to_num(U32 val);
U32  num_to_u32(Expr val);

Expr u64_to_num(U64 val);
U64  num_to_u64(Expr val);

U64  num_to_i64(Expr val);

Expr size_to_num(size_t val);
size_t num_to_size(Expr val);

Expr f_coerce(Expr exp, Expr type);

/* util *******/

/* all f_* functions:
 - always return an Expr (possibly NIL)
 - only have Expr arguments
 - don't take varargs (TODO?)
*/

Bool is_tagged(Expr exp, Expr tag);

inline static Bool is_op(Expr exp, Expr sym)
{
    return is_cons(exp) && eq(car(exp), sym);
}

inline static Bool is_quote(Expr exp)
{
    return is_op(exp, SYM_quote);
}

#if ENABLE_BACKQUOTE

inline static Bool is_backquote(Expr exp)
{
    return is_op(exp, SYM_backquote);
}

inline static Bool is_unquote(Expr exp)
{
    return is_op(exp, SYM_unquote);
}

#endif

Expr make_truth(Bool b);

Bool equal(Expr a, Expr b);

void rformat(char const * fmt, ...);

Expr f_eq(Expr a, Expr b);
Expr f_equal(Expr a, Expr b);

Expr f_symbol_name(Expr exp);

inline static Expr f_cons(Expr a, Expr b)
{
    return make_cons(a, b);
}

Expr f_type(Expr exp);

inline static Expr f_eval(Expr exp, Expr env)
{
    return eval(exp, env);
}

inline static Expr f_apply(Expr op, Expr vals, Expr env)
{
    return apply(op, vals, env);
}

Expr f_load_file(Expr ifn, Expr env);
Expr f_load_once(Expr ifn, Expr env);
Expr f_read_file(Expr ifn);

Expr f_escape(Expr str);

/* hash_impl **/

void make_hash_set(HashSetU64 * set);
void free_hash_set(HashSetU64 * set);

Bool hash_set_has(HashSetU64 const * set, U64 value);
void hash_set_put(HashSetU64 * set, U64 value);

void hash_set_clear(HashSetU64 * set);

#if DEBUG
void show_set(HashSetU64 const * set);
#endif

U64 hash_u64(U64 value);
U64 hash_str(char const * str);

/* spooky *****/

U32 spooky_hash32(size_t length, void const * message, U32 seed);
U64 spooky_hash64(size_t length, void const * message, U64 seed);

/* time *******/

F64 get_time();

class Profiler
{
public:
    inline Profiler(F64 * var) : var(var)
    {
        t0 = get_time();
    }

    inline Profiler(F64 * var, U64 * cnt) : var(var)
    {
        t0 = get_time();
        if (cnt)
        {
            ++*cnt;
        }
    }

    inline ~Profiler()
    {
        F64 const t1 = get_time();
        F64 const dt = t1 - t0;
        *var += dt;
    }

private:
    F64 * var;
    F64 t0;
};

/* meta *******/

void meta_install(Expr env);

/* system *****/

struct System
{
    Symbol symbol;
    ConsBuffer cons;
#if ENABLE_GENSYM
    Gensym gensym;
#endif
};

extern System g_sys; // TODO remove this eventually

void system_init();
void system_quit();

void system_bind_core(Expr env);
void system_bind_sdl2(Expr env);

void system_test(Expr suite, Bool show_pass);

void system_repl(Expr env);
void system_eval(char const * src, Expr env);
void system_load(char const * ifn, Expr env);
