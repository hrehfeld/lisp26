
#include "lisp.hpp"

#if defined(_WIN32)

static U64 get_timer_frequency()
{
    LARGE_INTEGER value;
    ::QueryPerformanceFrequency(&value);
    return (U64) value.QuadPart;
}

static U64 get_timer_value()
{
    LARGE_INTEGER value;
    ::QueryPerformanceCounter(&value);
    return (U64) value.QuadPart;
}

#elif defined(__unix__) || defined(__APPLE__)

#include <time.h>

static U64 get_timer_frequency()
{
    return 1000000000ull; // nano-seconds
}

static U64 get_timer_value()
{
    struct timespec time;
    clock_gettime(CLOCK_REALTIME, &time);
    return (U64) time.tv_sec * 1000000000ull + (U64) time.tv_nsec;
}

#else

static U64 get_timer_frequency()
{
    return CLOCKS_PER_SEC;
}

static U64 get_timer_value()
{
    return (U64) clock();
}

#endif

F64 get_time()
{
    return (F64) get_timer_value() / (F64) get_timer_frequency();
}
