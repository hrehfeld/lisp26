
#include "lisp.hpp"

typedef union
{
    U32 u;
    I32 i;
    F32 f;
} V32;

typedef union
{
    U64 u;
    I64 i;
    F64 f;
} V64;

I32 u32_as_i32(U32 value)
{
    V32 v;
    v.u = value;
    return v.i;
}

F32 u32_as_f32(U32 value)
{
    V32 v;
    v.u = value;
    return v.f;
}

U32 i32_as_u32(I32 value)
{
    V32 v;
    v.i = value;
    return v.u;
}

F32 i32_as_f32(I32 value)
{
    V32 v;
    v.i = value;
    return v.f;
}

U32 f32_as_u32(F32 value)
{
    V32 v;
    v.f = value;
    return v.u;
}

I32 f32_as_i32(F32 value)
{
    V32 v;
    v.f = value;
    return v.i;
}

I64 u64_as_i64(U64 value)
{
    V64 v;
    v.u = value;
    return v.i;
}

F64 u64_as_f64(U64 value)
{
    V64 v;
    v.u = value;
    return v.f;
}

U64 i64_as_u64(I64 value)
{
    V64 v;
    v.i = value;
    return v.u;
}

F64 i64_as_f64(I64 value)
{
    V64 v;
    v.i = value;
    return v.f;
}

U64 f64_as_u64(F64 value)
{
    V64 v;
    v.f = value;
    return v.u;
}

I64 f64_as_i64(F64 value)
{
    V64 v;
    v.f = value;
    return v.i;
}

#if ENABLE_FIXNUM

Expr int_to_num(int val)
{
    return make_fixnum((I64) val);
}

int num_to_int(Expr val)
{
    return (int) fixnum_value(val);
}

Expr u32_to_num(U32 val)
{
    return make_fixnum((I64) val);
}

U32 num_to_u32(Expr val)
{
    I64 i64 = fixnum_value(val);
    U32 u32 = (U32) i64;
#if CHECK_OVERFLOWS
    if ((I64) u32 != i64)
    {
        ERROR("overflow");
    }
#endif
    return u32;
}

I32 num_to_i32(Expr val)
{
    I64 i64 = fixnum_value(val);
    I32 i32 = (I32) i64;
#if CHECK_OVERFLOWS
    if ((I64) i32 != i64)
    {
        ERROR("overflow");
    }
#endif
    return i32;
}

Expr u64_to_num(U64 val)
{
    I64 i64 = (I64) val;
#if CHECK_OVERFLOWS
    if (i64 > FIXNUM_MAXVAL)
    {
        ERROR("overflow");
    }
#endif
    return make_fixnum(i64);
}

U64 num_to_u64(Expr val)
{
    return (U64) fixnum_value(val);
}

I64 num_to_i64(Expr val)
{
    return (I64) fixnum_value(val);
}

Expr size_to_num(size_t val)
{
    return u64_to_num((U64) val);
}

size_t num_to_size(Expr val)
{
    return (size_t) fixnum_value(val);
}

#else

#define ERROR_NO_FIXNUM ERROR("try to build with ENABLE_FIXNUM")

Expr int_to_num(int val)
{
    return ERROR_NO_FIXNUM;
}

int num_to_int(Expr val)
{
    ERROR_NO_FIXNUM;
    return 0;
}

U32 num_to_u32(Expr val)
{
    ERROR_NO_FIXNUM;
    return 0;
}

U64 num_to_u64(Expr val)
{
    ERROR_NO_FIXNUM;
    return 0;
}

U64 num_to_i64(Expr val)
{
    ERROR_NO_FIXNUM;
    return 0;
}

Expr u32_to_num(U32 val)
{
    return ERROR_NO_FIXNUM;
}

Expr u64_to_num(U64 val)
{
    return ERROR_NO_FIXNUM;
}

Expr size_to_num(size_t val)
{
    return u64_to_num((U64) val);
}

size_t num_to_size(Expr val)
{
    ERROR_NO_FIXNUM;
    return 0;
}

#endif

Expr f_coerce(Expr exp, Expr type)
{
    if (f_type(exp) == type)
    {
        return exp;
    }
    if (type == SYM_string)
    {
        return repr_as_expr(exp);
    }
    return ERROR("cannot coerce %s to %s", repr(exp), repr(type));
}
