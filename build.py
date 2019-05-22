
import os, sys, random

def shell_exec(x):
    print(x)
    ret = os.system(x)
    if ret:
        sys.exit(ret)
    return ret

def test():
    return [["foo"], "bar"]

def build(enable_hash=1):
    for name in ["gensym", "time"]:
        shell_exec("g++ -c -std=c++11 -Wall -O2" + " -DENABLE_HASH=" + str(enable_hash) + " " + name + ".cpp" + " -o " + name + ".o")
    shell_exec("g++ -Wall -O2 bignum.o builtin.o builtin_misc.o closure.o coerce.o cons.o core.o env.o error.o eval.o expr.o fixnum.o float.o gensym.o hash.o hash_impl.o list.o meta.o number.o pointer.o printer.o reader.o sdl2.o spooky.o stream.o stream_impl.o string.o symbol.o system.o test.o time.o util.o vector.o lisp.o -lSDL2 -o lisp")

if __name__ == "__main__":
    build(enable_hash = random.randint(0, 1))
