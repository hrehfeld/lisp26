
#include "lisp.hpp"

/***********************************************************

cleaned-up version of Bob Jenkins' public domain hash

source: http://www.burtleburtle.net/bob/hash/spooky.html

***********************************************************/

#define sc_numVars   12
#define sc_blockSize (sc_numVars * 8)
#define sc_bufSize   (2 * sc_blockSize)
#define sc_const     U64VAL(0xdeadbeefdeadbeef)

static inline U64 Rot64(U64 x, int k)
{
    return (x << k) | (x >> (64 - k));
}

#define Mix(data, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11)\
{\
    s0  += data[0];   s2  ^= s10;  s11 ^= s0;   s0  = Rot64(s0 , 11);  s11 += s1;\
    s1  += data[1];   s3  ^= s11;  s0  ^= s1;   s1  = Rot64(s1 , 32);  s0  += s2;\
    s2  += data[2];   s4  ^= s0;   s1  ^= s2;   s2  = Rot64(s2 , 43);  s1  += s3;\
    s3  += data[3];   s5  ^= s1;   s2  ^= s3;   s3  = Rot64(s3 , 31);  s2  += s4;\
    s4  += data[4];   s6  ^= s2;   s3  ^= s4;   s4  = Rot64(s4 , 17);  s3  += s5;\
    s5  += data[5];   s7  ^= s3;   s4  ^= s5;   s5  = Rot64(s5 , 28);  s4  += s6;\
    s6  += data[6];   s8  ^= s4;   s5  ^= s6;   s6  = Rot64(s6 , 39);  s5  += s7;\
    s7  += data[7];   s9  ^= s5;   s6  ^= s7;   s7  = Rot64(s7 , 57);  s6  += s8;\
    s8  += data[8];   s10 ^= s6;   s7  ^= s8;   s8  = Rot64(s8 , 55);  s7  += s9;\
    s9  += data[9];   s11 ^= s7;   s8  ^= s9;   s9  = Rot64(s9 , 54);  s8  += s10;\
    s10 += data[10];  s0  ^= s8;   s9  ^= s10;  s10 = Rot64(s10, 22);  s9  += s11;\
    s11 += data[11];  s1  ^= s9;   s10 ^= s11;  s11 = Rot64(s11, 46);  s10 += s0;\
}

#define EndPartial(h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11)\
{\
    h11+= h1;    h2 ^= h11;   h1 = Rot64(h1,44);\
    h0 += h2;    h3 ^= h0;    h2 = Rot64(h2,15);\
    h1 += h3;    h4 ^= h1;    h3 = Rot64(h3,34);\
    h2 += h4;    h5 ^= h2;    h4 = Rot64(h4,21);\
    h3 += h5;    h6 ^= h3;    h5 = Rot64(h5,38);\
    h4 += h6;    h7 ^= h4;    h6 = Rot64(h6,33);\
    h5 += h7;    h8 ^= h5;    h7 = Rot64(h7,10);\
    h6 += h8;    h9 ^= h6;    h8 = Rot64(h8,13);\
    h7 += h9;    h10^= h7;    h9 = Rot64(h9,38);\
    h8 += h10;   h11^= h8;    h10= Rot64(h10,53);\
    h9 += h11;   h0 ^= h9;    h11= Rot64(h11,42);\
    h10+= h0;    h1 ^= h10;   h0 = Rot64(h0,54);\
}

#define End(data, h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11)\
{\
    h0 += data[0];   h1 += data[1];   h2 += data[2];   h3 += data[3];\
    h4 += data[4];   h5 += data[5];   h6 += data[6];   h7 += data[7];\
    h8 += data[8];   h9 += data[9];   h10 += data[10]; h11 += data[11];\
    EndPartial(h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);\
    EndPartial(h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);\
    EndPartial(h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);\
}

#define ShortMix(h0, h1, h2, h3)\
{\
    h2 = Rot64(h2, 50);  h2 += h3;  h0 ^= h2;\
    h3 = Rot64(h3, 52);  h3 += h0;  h1 ^= h3;\
    h0 = Rot64(h0, 30);  h0 += h1;  h2 ^= h0;\
    h1 = Rot64(h1, 41);  h1 += h2;  h3 ^= h1;\
    h2 = Rot64(h2, 54);  h2 += h3;  h0 ^= h2;\
    h3 = Rot64(h3, 48);  h3 += h0;  h1 ^= h3;\
    h0 = Rot64(h0, 38);  h0 += h1;  h2 ^= h0;\
    h1 = Rot64(h1, 37);  h1 += h2;  h3 ^= h1;\
    h2 = Rot64(h2, 62);  h2 += h3;  h0 ^= h2;\
    h3 = Rot64(h3, 34);  h3 += h0;  h1 ^= h3;\
    h0 = Rot64(h0, 5);   h0 += h1;  h2 ^= h0;\
    h1 = Rot64(h1, 36);  h1 += h2;  h3 ^= h1;\
}

#define ShortEnd(h0, h1, h2, h3)\
{\
    h3 ^= h2;  h2 = Rot64(h2, 15);  h3 += h2;\
    h0 ^= h3;  h3 = Rot64(h3, 52);  h0 += h3;\
    h1 ^= h0;  h0 = Rot64(h0, 26);  h1 += h0;\
    h2 ^= h1;  h1 = Rot64(h1, 51);  h2 += h1;\
    h3 ^= h2;  h2 = Rot64(h2, 28);  h3 += h2;\
    h0 ^= h3;  h3 = Rot64(h3, 9);   h0 += h3;\
    h1 ^= h0;  h0 = Rot64(h0, 47);  h1 += h0;\
    h2 ^= h1;  h1 = Rot64(h1, 54);  h2 += h1;\
    h3 ^= h2;  h2 = Rot64(h2, 32);  h3 += h2;\
    h0 ^= h3;  h3 = Rot64(h3, 25);  h0 += h3;\
    h1 ^= h0;  h0 = Rot64(h0, 63);  h1 += h0;\
}

static inline void Short(size_t length, void const * message, U64 * hash1, U64 * hash2)
{
    union
    {
        U8  const * p8;
        U32       * p32;
        U64       * p64;
        size_t      i;
    } u;

    u.p8 = (U8 const *) message;

    size_t remainder = length % 32;
    U64 a = *hash1;
    U64 b = *hash2;
    U64 c = sc_const;
    U64 d = sc_const;

    if (length > 15)
    {
        U64 const * end = u.p64 + (length / 32) * 4;

        /* handle all complete sets of 32 bytes */
        for (; u.p64 < end; u.p64 += 4)
        {
            c += u.p64[0];
            d += u.p64[1];
            ShortMix(a, b, c, d);
            a += u.p64[2];
            b += u.p64[3];
        }

        /* handle the case of 16+ remaining bytes */
        if (remainder >= 16)
        {
            c += u.p64[0];
            d += u.p64[1];
            ShortMix(a, b, c, d);
            u.p64 += 2;
            remainder -= 16;
        }
    }

    /* handle the last 0..15 bytes, and its length */
    d += (U64) length << 56;
    switch (remainder)
    {
    case 15:
        d += (U64) u.p8[14] << 48;
    case 14:
        d += (U64) u.p8[13] << 40;
    case 13:
        d += (U64) u.p8[12] << 32;
    case 12:
        d += u.p32[2];
        c += u.p64[0];
        break;
    case 11:
        d += (U64) u.p8[10] << 16;
    case 10:
        d += (U64) u.p8[9] << 8;
    case 9:
        d += (U64) u.p8[8];
    case 8:
        c += u.p64[0];
        break;
    case 7:
        c += (U64) u.p8[6] << 48;
    case 6:
        c += (U64) u.p8[5] << 40;
    case 5:
        c += (U64) u.p8[4] << 32;
    case 4:
        c += u.p32[0];
        break;
    case 3:
        c += (U64) u.p8[2] << 16;
    case 2:
        c += (U64) u.p8[1] << 8;
    case 1:
        c += (U64) u.p8[0];
        break;
    case 0:
        c += sc_const;
        d += sc_const;
    }

    ShortEnd(a, b, c, d);
    *hash1 = a;
    *hash2 = b;
}

static inline void NotSoShort(size_t length, void const * message, U64 * hash1, U64 * hash2)
{
    U64 h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11;
    U64 buf[sc_numVars];
    U64 * end;
    union
    {
        U8 const * p8;
        U64 *      p64;
        size_t     i;
    } u;
    size_t remainder;

    h0 = h3 = h6 = h9  = *hash1;
    h1 = h4 = h7 = h10 = *hash2;
    h2 = h5 = h8 = h11 = sc_const;

    u.p8 = (U8 const *) message;
    end = u.p64 + (length / sc_blockSize) * sc_numVars;

    while (u.p64 < end)
    {
        Mix(u.p64, h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);
        u.p64 += sc_numVars;
    }

    remainder = (length - ((U8 const *) end - (U8 const *) message));
    memcpy(buf, end, remainder);
    memset(((U8 *) buf) + remainder, 0, sc_blockSize - remainder);
    ((U8 *) buf)[sc_blockSize - 1] = remainder;

    End(buf, h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);
    *hash1 = h0;
    *hash2 = h1;
}

static void Hash128(size_t length, void const * message, U64 * hash1, U64 * hash2)
{
    if (length < sc_bufSize)
    {
        Short(length, message, hash1, hash2);
    }
    else
    {
        NotSoShort(length, message, hash1, hash2);
    }
}

static U32 Hash32(size_t length, void const * message, U32 seed)
{
    U64 hash1 = seed, hash2 = seed;
    Hash128(length, message, &hash1, &hash2);
    return (U32) hash1;
}

static U64 Hash64(size_t length, void const * message, U64 seed)
{
    U64 hash1 = seed;
    Hash128(length, message, &hash1, &seed);
    return hash1;
}

U32 spooky_hash32(size_t length, void const * message, U32 seed)
{
    return Hash32(length, message, seed);
}

U64 spooky_hash64(size_t length, void const * message, U64 seed)
{
    return Hash64(length, message, seed);
}
