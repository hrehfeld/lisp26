
#pragma once

#define _RED     "\x1b[31m"
#define _GREEN   "\x1b[32m"
#define _YELLOW  "\x1b[33m"
#define _BLUE    "\x1b[34m"
#define _MAGENTA "\x1b[35m"
#define _CYAN    "\x1b[36m"
#define _RESET   "\x1b[0m"

#define _TEST_FILE stdout

#define TEST_GROUP(text)         fprintf(_TEST_FILE, "==== " text " ====\n")

#define TEST_BEGIN()\
num_tests = 0;\
num_failed = 0

#define TEST_FINISH()\
if (num_failed)\
{\
    fprintf(_TEST_FILE, _RED   "FAIL" _RESET " " "%d/%d test(s)\n", num_failed, num_tests); \
}\
else\
{\
    fprintf(_TEST_FILE, _GREEN "PASS" _RESET " " "%d/%d test(s)\n", num_tests, num_tests); \
}

#if TEST_SHOW_PASS

#define TEST_ASSERT_MSG(exp, msg)                   \
do\
{\
    try\
    {\
        ++num_tests;\
        if (exp)\
        {\
            fprintf(_TEST_FILE, _GREEN "PASS" _RESET " " msg "\n");  \
        }\
        else\
        {\
            ++num_failed;\
            fprintf(_TEST_FILE, _RED   "FAIL" _RESET " " msg "\n");\
        }\
    }\
    catch (...)\
    {\
        ++num_failed;\
        fprintf(_TEST_FILE, _RED   "FAIL" _RESET " " msg "\n");\
    }\
} while (0)

#else

#define TEST_ASSERT_MSG(exp, msg)                   \
do\
{\
    try\
    {\
        ++num_tests;\
        if (exp)\
        {\
        }\
        else\
        {\
            ++num_failed;\
            fprintf(_TEST_FILE, _RED   "FAIL" _RESET " " msg "\n");\
        }\
    }\
    catch (Expr err)\
    {\
        ++num_failed;\
        fprintf(_TEST_FILE, _RED   "FAIL" _RESET " " msg " => %s\n", repr(err)); \
    }\
    catch (...)\
    {\
        ++num_failed;\
        fprintf(_TEST_FILE, _RED   "FAIL" _RESET " " msg "\n");\
    }\
} while (0)

#endif

#define TEST_ASSERT(exp) TEST_ASSERT_MSG(exp, #exp)
