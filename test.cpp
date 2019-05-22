
#include "lisp.hpp"

#include "test.hpp"

static int num_tests = 0;
static int num_failed = 0;

static Expr env = nil;

Bool eval_eq(char const * src, char const * ref)
{
    return eq(eval(read_from_string(src), env), read_from_string(ref));
}

static Bool eval_equal(char const * src, char const * ref)
{
    return equal(eval(read_from_string(src), env), read_from_string(ref));
}

static void system_test_self()
{
    TEST_BEGIN();
    TEST_ASSERT(intern("nil") == nil);

#if ENABLE_FIXNUM
    TEST_ASSERT(is_fixnum(read_from_string("123")));
#endif
    TEST_ASSERT(is_string(read_from_string("\"123\"")));

    TEST_ASSERT(eval(nil, env) == nil);
    TEST_ASSERT(eval(LIST2(QUOTE(quote), QUOTE(foo)), env) == QUOTE(foo));

    TEST_ASSERT(eval_equal("nil", "nil"));
    TEST_ASSERT(eval_equal("123", "123"));
    TEST_ASSERT(eval_equal("\"foo\"", "\"foo\""));

#if ENABLE_FIXNUM
    {
        Expr c = nil;

        TEST_ASSERT( fixnum_maybe_add(make_fixnum(FIXNUM_MAXVAL), make_fixnum(0), &c));
        TEST_ASSERT(!fixnum_maybe_add(make_fixnum(FIXNUM_MAXVAL), make_fixnum(1), &c));

        TEST_ASSERT( fixnum_maybe_sub(make_fixnum(FIXNUM_MINVAL), make_fixnum(0), &c));
        TEST_ASSERT(!fixnum_maybe_sub(make_fixnum(FIXNUM_MINVAL), make_fixnum(1), &c));
    }
#endif

    TEST_FINISH();
}

static void run_test_file(char const * path, Expr env, Bool show_pass)
{
    int num_tests = 0;
    int num_failed = 0;
    Expr const tests = read_file(path);
    for (Expr iter = tests; iter; iter = cdr(iter))
    {
        Expr const test = car(iter);
        Expr const src = car(test);
        Expr const ref = caddr(test);
#if TEST_CATCH_ERRORS
        try
#endif
        {
            ++num_tests;
            //printf(_YELLOW "TEST" _RESET " %s\n", repr(src));
            Expr const val = eval(src, env);
            if (equal(val, ref))
            {
                if (show_pass)
                {
                    printf(_GREEN "PASS" _RESET " %s => %s\n", repr(src), repr(ref));
                }
            }
            else
            {
                ++num_failed;
                printf(_RED "FAIL" _RESET " %s => %s != %s\n", repr(src), repr(val), repr(ref));
#if TEST_STOP_ON_FAIL
                break;
#endif
            }
        }
#if TEST_CATCH_ERRORS
        catch (Expr err)
        {
            ++num_failed;
            printf(_BLUE "JUNK" _RESET " %s => %s != %s\n", repr(src), repr(err), repr(ref));
        }
#endif
    }

    if (num_failed)
    {
        printf(_RED "FAIL" _RESET " %d/%d test(s)\n", num_failed, num_tests);
    }
    else
    {
        printf(_GREEN "PASS" _RESET " %d/%d test(s)\n", num_tests, num_tests);
    }
}

static void system_test_file(Expr suite, Bool show_pass)
{
    Expr env = make_env(nil);
    system_bind_core(env);
    env_def(env, QUOTE(*argv*), nil);

    for (Expr iter = suite; iter; iter = cdr(iter))
    {
        char const * ifn = string_value(car(iter));
        if (cdr(iter))
        {
            load_file(ifn, env);
        }
        else
        {
            run_test_file(ifn, env, show_pass);
        }
    }
}

void system_test(Expr suite, Bool show_pass)
{
    if (suite)
    {
        system_test_file(suite, show_pass);
    }
    else
    {
        reader_test();
        env_test();
        system_test_self();
    }
}
