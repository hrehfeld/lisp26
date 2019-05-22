
#include "lisp.hpp"
#include "test.hpp"

#define TEST_ASSERT_EQUAL(a, b) TEST_ASSERT(equal(a, b))

void reader_test()
{
    int num_tests = 0;
    int num_failed = 0;

    Expr const attr = intern("attr");
    Expr const foo  = intern("foo");
    Expr const a    = intern("a");
    Expr const b    = intern("b");
    Expr const c    = intern("c");

    TEST_BEGIN();
    TEST_GROUP("reader");
    TEST_ASSERT_EQUAL(read_from_string("nil"), nil);
    TEST_ASSERT_EQUAL(read_from_string("()"), nil);
    TEST_ASSERT_EQUAL(read_from_string("foo"), foo);
    TEST_ASSERT_EQUAL(read_from_string("(a . b)"), cons(a, b));

    // TODO
    TEST_ASSERT_EQUAL(read_from_string("a.b"), list(attr, a, b));
    TEST_ASSERT_EQUAL(read_from_string("a.b.c"), list(attr, a, b, c));

    TEST_FINISH();
}
