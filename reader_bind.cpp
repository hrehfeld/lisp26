
#include "lisp.hpp"

static Expr fun_read(Expr args, Expr env, void * user)
{
    if (!unpack_all_args(args, ""))
    {
        return ERROR("illegal arguments");
    }
    return read_with_env(env);
}

static Expr fun_read_file(Expr args, Expr env, void * user)
{
    return f_read_file(car(args));
}

static Expr fun_read_from_stream(Expr args, Expr env, void * user)
{
    return read_from_stream(car(args));
}

static Expr fun_read_all_from_stream(Expr args, Expr env, void * user)
{
    return read_all_from_stream(car(args));
}

void reader_bind(Expr env)
{
    ENV_DEF_BUILTIN(env, QUOTE(read                ), fun_read                );
    ENV_DEF_BUILTIN(env, QUOTE(read-file           ), fun_read_file           );
    ENV_DEF_BUILTIN(env, QUOTE(read-from-stream    ), fun_read_from_stream    );
    ENV_DEF_BUILTIN(env, QUOTE(read-all-from-stream), fun_read_all_from_stream);
}
