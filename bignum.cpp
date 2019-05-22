
#include "lisp.hpp"

#if ENABLE_BIGNUM

#include <vector>
#include <string>

class BigInt
{
public:
    BigInt()
    {
    }

    BigInt(I64 value)
    {
        *this = value;
    }

    BigInt & operator=(I64 value)
    {
        if (words.size() < 1)
        {
            words.resize(1);
        }

        memset(words.data(), -(value < 0), words.size() * sizeof(U64));
        words[0] = (U64) value;

        return *this;
    }

    Bool is_i64() const
    {
        return words.size() <= 1;
    }

    I64 to_i64() const
    {
        return u64_as_i64(words[0]);
    }

    std::string to_string() const
    {
        return 0; // TODO
    }

    std::vector<U64> words;
};

BigInt operator+(BigInt const & a, BigInt const & b)
{
    return BigInt(0); // TODO
}

BigInt operator-(BigInt const & a, BigInt const & b)
{
    return BigInt(0); // TODO
}

BigInt operator*(BigInt const & a, BigInt const & b)
{
    return BigInt(0); // TODO
}

/* ****** */

static std::vector<BigInt> bignums_;

static Expr _make_bignum(BigInt && val)
{
    U64 const idx = (U64) bignums_.size();
    bignums_.push_back(val);
    return make_expr(TYPE_BIGNUM, idx);
}

#if 0
static BigInt & _ref_bignum(Expr exp)
{
    ASSERT(is_bignum(exp));
    U64 const idx = expr_data(exp);
    return bignums_[idx];
}
#endif

static BigInt const & _cref_bignum(Expr exp)
{
    ASSERT(is_bignum(exp));
    U64 const idx = expr_data(exp);
    return bignums_[idx];
}

void bignum_init()
{
}

void bignum_quit()
{
    bignums_.clear();
}

void bignum_gc()
{
    /* TODO */
}

static Expr b_fixnum_to_bignum(Expr args, Expr env, void * user)
{
    return fixnum_to_bignum(car(args));
}

void bignum_install(Expr env)
{
    env_def(env, QUOTE(fixnum->bignum), make_builtin_fun(b_fixnum_to_bignum, NULL));
}

void bignum_print(PrintFun rec, Expr out, Expr exp)
{
    stream_put_cstring(out, _cref_bignum(exp).to_string().c_str());
}

Bool is_bignum(Expr exp)
{
    return expr_type(exp) == TYPE_BIGNUM;
}

Expr make_bignum(I64 value)
{
    return _make_bignum(BigInt(value));
}

Bool bignum_is_fixnum(Expr exp)
{
#if ENABLE_FIXNUM
    auto const & num = _cref_bignum(exp);
    if (!num.is_i64())
    {
        return 0;
    }

    I64 val = num.to_i64();
    if (val < FIXNUM_MINVAL)
    {
        return 0;
    }

    if (val > FIXNUM_MAXVAL)
    {
        return 0;
    }

    return 1;
#else
    return 0;
#endif
}

I64 bignum_value(Expr exp)
{
    auto const & num = _cref_bignum(exp);
    if (!num.is_i64())
    {
        ERROR("overflow");
    }

    return num.to_i64();
}

Expr fixnum_to_bignum(Expr exp)
{
#if ENABLE_FIXNUM
    return make_bignum(fixnum_value(exp));
#else
    return ERROR("not a fixnum");
#endif
}

Expr bignum_add(Expr a, Expr b)
{
    ASSERT(is_bignum(a));
    ASSERT(is_bignum(b));
    return _make_bignum(_cref_bignum(a) + _cref_bignum(b));
}

Expr bignum_sub(Expr a, Expr b)
{
    ASSERT(is_bignum(a));
    ASSERT(is_bignum(b));
    return _make_bignum(_cref_bignum(a) - _cref_bignum(b));
}

Expr bignum_mul(Expr a, Expr b)
{
    ASSERT(is_bignum(a));
    ASSERT(is_bignum(b));
    return _make_bignum(_cref_bignum(a) * _cref_bignum(b));
}

#endif
