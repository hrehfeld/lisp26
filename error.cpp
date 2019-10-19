
#include "lisp.hpp"

#include <vector>
#include <string>

#include <stdio.h>

static std::vector<std::string> _error_info;

ErrorContext::ErrorContext(char const * fmt, ...)
{
    // TODO we need a formatting facility
    static char info[1000];
    va_list ap;
    va_start(ap, fmt);
    vsprintf(info, fmt, ap);
    va_end(ap);
    _error_info.push_back(std::string(info));
}

ErrorContext::~ErrorContext()
{
    _error_info.pop_back();
}

Bool is_error(Expr exp)
{
    return exp == SYM_ERROR ||
        (is_cons(exp) && car(exp) == SYM_ERROR);
}

Expr make_error(char const * file, int line, char const * fmt, ...)
{
    //printf("%s:%d: generated an error\n", file, line);
#if PRINT_ON_ERROR
    Expr val = SYM_ERROR;
    if (fmt)
    {
        va_list ap;
        va_start(ap, fmt);
        vfprintf(ERROR_FILE, fmt, ap);
        va_end(ap);
        fprintf(ERROR_FILE, "\n");
    }
#else
    Expr val = nil;
    if (fmt)
    {
        char msg[2048]; // TODO size?
        va_list ap;
        va_start(ap, fmt);
        vsprintf(msg, fmt, ap);
        va_end(ap);
        val = cons(make_string(msg), val);
    }
    val = cons(SYM_ERROR, val);
#endif

#if EXIT_ON_ERROR
    assert(0);
    exit(1);
    return val;
#else
    throw val;
#endif
}

void show_backtrace()
{
#if ENABLE_BACKTRACE
    void * buffer[MAX_STACK_FRAMES];
    int const nframes = backtrace(buffer, MAX_STACK_FRAMES);
    char ** strings = backtrace_symbols(buffer, nframes);
    if (nframes)
    {
        fprintf(ERROR_FILE, "BACKTRACE:\n");
        //if (strings)
        {
            for (int i = 0; i < nframes; ++i)
            {
                fprintf(ERROR_FILE, "%s\n", strings[i]);
            }
            free(strings);
        }
    }
#endif
#if EVAL_STACK_MARKER
    extern Expr _eval_stack;
    //fprintf(ERROR_FILE, "EVAL STACK: %s", repr(_eval_stack));
    if (_eval_stack)
    {
        fprintf(ERROR_FILE, "EVAL STACK:\n");
        for (Expr it = _eval_stack; it; it = cdr(it))
        {
            // TODO need to collect names for better output than e.g.
            // - #:<core function>
            // - #:<lisp macro>
            // - #:<lisp function>
            Expr const op = car(it);
            fprintf(ERROR_FILE, "- %s\n", repr(op));
        }
    }
#endif
}

void show_error_context()
{
    if (!_error_info.empty())
    {
        fprintf(ERROR_FILE, "CONTEXT:\n");
        for (auto info : _error_info)
        {
            fprintf(ERROR_FILE, "%s\n", info.c_str());
        }
    }
}
