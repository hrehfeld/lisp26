
#include "lisp.hpp"
#include "test.hpp"

void env_test()
{
    int num_tests = 0;
    int num_failed = 0;

    TEST_BEGIN();
    TEST_GROUP("env");
    {
        Expr env = make_env(nil);
        Expr foo = intern("foo");

        TEST_ASSERT(env);
        TEST_ASSERT(!env_has(env, foo));
        env_def(env, foo, nil);
        TEST_ASSERT(env_has(env, foo));
        TEST_ASSERT(equal(env_get(env, foo), nil));
    }
    {
        Expr env = make_env(nil);
        Expr foo = intern("foo");
        Expr bar = intern("bar");

        TEST_ASSERT(env);
        TEST_ASSERT(!env_has(env, foo));
        TEST_ASSERT(!env_has(env, bar));
        env_def(env, foo, bar);
        TEST_ASSERT(env_has(env, foo));
        TEST_ASSERT(!env_has(env, bar));
        TEST_ASSERT(equal(env_get(env, foo), bar));
    }
    {
        Expr inner = make_env(nil);
        Expr outer = make_env(inner);
        Expr foo = intern("foo");
        Expr bar = intern("bar");

        env_def(inner, foo, bar);
        TEST_ASSERT(env_has(inner, foo));
        TEST_ASSERT(env_has(outer, foo));

        TEST_ASSERT(env_owns(inner, foo));
        TEST_ASSERT(!env_owns(outer, foo));

        TEST_ASSERT(equal(env_get(inner, foo), bar));
        TEST_ASSERT(equal(env_get(outer, foo), bar));
    }
    TEST_FINISH();
}
