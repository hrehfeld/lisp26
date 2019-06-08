
#include "lisp.hpp"

#define PROFILE (PROFILE_INTERN || PROFILE_LOOKUP)

#if PROFILE
F64 _main_time = 0.0;
#endif

static int lisp_main(int argc, char ** argv)
{
#if PROFILE
    Profiler prof(&_main_time, NULL);
#endif
    int ret = 0;
    system_init();

    try
    {
        Expr env = nil;

        env = make_env(nil);

        char const * cmd = NULL;
        char const * ifn = NULL;

        Bool bind_args = 1;
        Bool load_core = 1;
        Bool load_sdl2 = 0;

        Bool show_pass = 0;

        Expr args = nil;
        for (int i = 1; i < argc; ++i)
        {
            if (!cmd)
            {
                if (!strcmp("--core", argv[i]))
                {
                    load_core = 1;
                }
                else if (!strcmp("--no-core", argv[i]))
                {
                    load_core = 0;
                }
                else if (!strcmp("--sdl2", argv[i]))
                {
                    load_sdl2 = 1;
                }
                else if (!strcmp("--no-sdl2", argv[i]))
                {
                    load_sdl2 = 0;
                }
                else if (!strcmp("--args", argv[i]))
                {
                    bind_args = 1;
                }
                else if (!strcmp("--no-args", argv[i]))
                {
                    bind_args = 0;
                }
                else if (!strcmp("--show-pass", argv[i]))
                {
                    show_pass = 1;
                }
                else
                {
                    cmd = argv[i];
                }
            }
            else if (!strcmp("repl", cmd))
            {
                args = cons(make_string(argv[i]), args);
            }
            else if (!strcmp("load", cmd))
            {
                if (!ifn)
                {
                    ifn = argv[i];
                }
                else
                {
                    args = cons(make_string(argv[i]), args);
                }
            }
            else if (!strcmp("test", cmd))
            {
                args = cons(make_string(argv[i]), args);
            }
            else
            {
                ERROR("illegal argument: %s\n", argv[i]);
            }
        }
        args = nreverse(args);
        if (bind_args)
        {
            env_def(env, QUOTE(*argv*), args);
        }

        if (load_core)
        {
            system_bind_core(env);
        }

        if (load_sdl2)
        {
            system_bind_sdl2(env);
        }

        if (!cmd)
        {
            fprintf(stderr, "missing command\n");
            fprintf(stderr, "usage: lisp {OPTION} (repl | load | test) {ARGUMENT}\n");
            ret = 1;
            goto done;
        }
        else if (!strcmp("repl", cmd))
        {
            system_repl(env);
        }
        else if (!strcmp("load", cmd))
        {
            system_load(ifn, env);
        }
        else if (!strcmp("test", cmd))
        {
            system_test(args, show_pass);
        }
        else
        {
            fprintf(stderr, "unrecognized command '%s'\n", cmd);
            ret = 1;
            goto usage;
        }
    }
    catch (Expr err)
    {
        fprintf(stderr, "unhandled error: %s\n", repr(err));
        show_backtrace();
        show_error_context();
        ret = 1;
    }

done:
    system_quit();

    return ret;

usage:
    fprintf(stderr, "usage: lisp {OPTION} (repl | load | test) {ARGUMENT}\n");
    goto done;
}

int main(int argc, char ** argv)
{
    int const ret = lisp_main(argc, argv);
#if PROFILE

    fprintf(stderr, "PROFILE:\n");
    fprintf(stderr, "TOTAL  %f %.3f%%\n", _main_time  , 100.0 * _main_time / _main_time);
#if PROFILE_INTERN
    extern F64 _intern_time;
    extern U64 _intern_call;
    fprintf(stderr, "INTERN %f %.3f%% %" PRIu64 "\n",
            _intern_time, 100.0 * _intern_time / _main_time,
            _intern_call);
#endif
#if PROFILE_LOOKUP
    extern F64 _lookup_time;
    extern U64 _lookup_call;
    fprintf(stderr, "LOOKUP %f %.3f%% %" PRIu64 "\n",
            _lookup_time, 100.0 * _lookup_time / _main_time,
            _lookup_call);
    extern U64 _max_vars;
    fprintf(stderr, "MAX VARS %" PRIu64 "\n", _max_vars);
#endif

#endif
    return ret;
}
