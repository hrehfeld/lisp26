
#include "lisp.hpp"

#define INITIAL_CAPACITY 8
#define LOAD_NUM         3
#define LOAD_DEN         4
#define POW2_ONLY        0

#define SPOOKY_SEED      0xeab470690e0f2f10llu

enum
{
    VALUE_EMPTY,
    VALUE_COUNT,
};

#if POW2_ONLY

static U64 mod(U64 x, U64 y)
{
    return x & (y - 1);
}

#else

static U64 mod(U64 x, U64 y)
{
    return x % y;
}

#endif

U64 xor_shift64(U64 state)
{
    state ^= (state >> 21);
    state ^= (state << 35);
    state ^= (state >> 4);
    return state;
}

U64 hash_u64(U64 x)
{
    return spooky_hash64(sizeof(U64), &x, SPOOKY_SEED);
}

U64 hash_str(char const * str) /* TODO pass in length */
{
    return spooky_hash64(strlen(str), str, SPOOKY_SEED);
}

/* TODO just use calloc? */
static void * malloc_zero(size_t size)
{
    void * ptr = malloc(size);
    if (ptr)
    {
        memset(ptr, 0, size);
    }
    return ptr;
}

static void make_new_set(HashSetU64 * set, U64 capacity)
{
    set->buffer = (U64 *) malloc_zero(sizeof(U64) * capacity);
    set->capacity = capacity;
    set->size = 0;
    set->bitset = 0;
}

static void rehash(HashSetU64 * set, U64 new_cap)
{
    U64 * buffer = set->buffer;
    U64 const capacity = set->capacity;
    U64 const bitset = set->bitset;
    make_new_set(set, new_cap);
    set->bitset = bitset;
    for (U64 i = 0; i < capacity; ++i)
    {
        U64 temp = buffer[i];
        if (temp >= VALUE_COUNT)
        {
            hash_set_put(set, temp);
        }
    }
    free(buffer);
}

static void maybe_rehash(HashSetU64 * set)
{
    U64 const next_size = set->size + 1;
    U64 const capacity = set->capacity;
    if (next_size * LOAD_DEN > capacity * LOAD_NUM)
    {
        rehash(set, capacity * 2);
    }
}

void make_hash_set(HashSetU64 * set)
{
    ASSERT(set);
    make_new_set(set, INITIAL_CAPACITY);
}

void free_hash_set(HashSetU64 * set)
{
    ASSERT(set);
    free(set->buffer);
    memset(set, 0, sizeof(HashSetU64));
}

Bool hash_set_has(HashSetU64 const * set, U64 value)
{
    DEBUG_ASSERT(set);
    if (value < VALUE_COUNT)
    {
        U64 const tmp1 = set->bitset;
        U64 const tmp2 = ((1llu << value) & tmp1);
        return tmp2 != 0;
    }
    else
    {
        {
            U64 const capacity = set->capacity;
            U64 const * buffer = set->buffer;
            U64 index = mod(hash_u64(value), capacity);
            while (1)
            {
                U64 const key = buffer[index];
                if (!key)
                {
                    return 0;
                }
                else if (key == value)
                {
                    return 1;
                }
                else
                {
                    index = mod(index + 1, capacity);
                }
            }
        }
    }
}

void hash_set_put(HashSetU64 * set, U64 value)
{
    DEBUG_ASSERT(set);
    if (value < VALUE_COUNT)
    {
        U64 const tmp1 = set->bitset;
        U64 const tmp2 = (1llu << value) | tmp1;
        set->size += tmp1 != tmp2;
        set->bitset = tmp2;
        return;
    }
    maybe_rehash(set);
    U64 const capacity = set->capacity;
    U64 * buffer = set->buffer;
    U64 index = mod(hash_u64(value), capacity);
    while (1)
    {
        U64 const key = buffer[index];
        if (!key)
        {
            buffer[index] = value;
            ++set->size;
            break;
        }
        else if (key == value)
        {
            break;
        }
        else
        {
            index = mod(index + 1, capacity);
        }
    }
}

void hash_set_clear(HashSetU64 * set)
{
    ASSERT(set);
    free_hash_set(set);
    make_hash_set(set);
}

void show_set(HashSetU64 const * set)
{
    printf("HASH SET:\n");
    for (U64 i = 0; i < set->capacity; ++i)
    {
        printf("%4" PRIu64 "\n", set->buffer[i]);
    }
    printf("size: %" PRIu64 "\n", set->size);
    printf("items:\n");
    for (U64 i = 0; i < VALUE_COUNT; ++i)
    {
        U64 const value = i;
        if (hash_set_has(set, value))
        {
            printf("%4" PRIu64 "\n", value);
        }
    }
    for (U64 i = 0; i < set->capacity; ++i)
    {
        U64 const value = set->buffer[i];
        if (value >= VALUE_COUNT)
        {
            printf("%4" PRIu64 "\n", value);
        }
    }
}
