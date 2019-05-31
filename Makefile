
DEBUG = 0

CXX = g++
CXXFLAGS = -std=c++11 -Wall -Wextra -Wno-implicit-fallthrough -Wno-unused-parameter -Wpedantic
# if we need to disable some more useless ones from extra
# -Wno-clobbered
# -Wno-cast-function-type
# -Wno-deprecated-copy (C++ only)
# -Wno-empty-body
# -Wno-ignored-qualifiers
# -Wno-missing-field-initializers
# -Wno-missing-parameter-type (C only)
# -Wno-old-style-declaration (C only)
# -Wno-override-init
# -Wno-sign-compare (C only)
# -Wno-redundant-move (only for C++)
# -Wno-type-limits
# -Wno-uninitialized
# -Wno-shift-negative-value (in C++03 and in C99 and newer)
# -Wno-unused-but-set-parameter (only with -Wunused or -Wall)
LDFLAGS = -Wall
LDLIBS = -lSDL2

ifeq ($(DEBUG), 1)
CXXFLAGS += -g
#CXXFLAGS += -rdynamic # TODO doesn't work with apple clang "g++"
CXXFLAGS += -O0
LDFLAGS += -O0
else
CXXFLAGS += -O2
LDFLAGS += -O2
endif

BIN = lisp lide
MOD = bignum builtin builtin_misc closure coerce cons cons_bind cons_impl core env env_install env_test error eval expr fixnum float gensym hash hash_bind hash_impl list meta number pointer printer reader reader_bind reader_test sdl2 spooky stream stream_impl string symbol system test time util vector
OBJ = $(MOD:%=%.o)
CPP = $(MOD:%=%.cpp)

PROG_LISP = $(wildcard tests/programs/*.lisp)
#PROG_PY   = $(PROG_LISP:%.lisp=%.py)
PROG_PY = tests/programs/hello.py tests/programs/blub.py
#PROG_CPP  = $(PROG_LISP:%.lisp=%.cpp)
PROG_CPP = tests/programs/hello.cpp tests/programs/blah.cpp tests/programs/comp.cpp
PROG_BIN  = $(PROG_CPP:%.cpp=%)

RUNTIME = runtime.hpp lisp.hpp config.hpp font.hpp

all: $(BIN)

clean:
	rm -rf $(BIN) $(OBJ)

# typing is hard
t: tests
e: extra
d: default-config
r: random-config

extra: $(PROG_BIN) $(PROG_PY)

tests: lisp std.lisp std.test
	./lisp test
	./lisp test core.test
	./lisp test env.test
	./lisp test std.lisp std.test
	./lisp test meta.lisp meta.test
	./lisp test hash.lisp hash.test
	./lisp test env.lisp env.test

default-config:
	python3 fuzz.py default

random-config:
	python3 fuzz.py random

%.o: %.cpp lisp.hpp config.hpp
	$(CXX) -c $(CXXFLAGS) $< -o $@

stream.o: stream.cpp lisp.hpp config.hpp stream_impl.hpp

stream_impl.o: stream_impl.cpp lisp.hpp config.hpp stream_impl.hpp

lisp: $(OBJ) lisp.o
	$(CXX) $(LDFLAGS) $^ $(LDLIBS) -o $@

lide: lide.cpp $(RUNTIME)
	$(CXX) $(CXXFLAGS) $< $(LDLIBS) -o $@

tests/programs/%.py: tests/programs/%.lisp lisp2py.lisp lisp2x.lisp std.lisp lisp
	./lisp load lisp2py.lisp --no-comments $< > $@

tests/programs/%.cpp: tests/programs/%.lisp lisp2cpp.lisp lisp2x.lisp std.lisp lisp
	./lisp load lisp2cpp.lisp $< > $@

# TODO merge comp into lisp2cpp and add --with-stub option to add main and runtime include
tests/programs/hello.cpp: tests/programs/hello.lisp comp.lisp
	./lisp load comp.lisp < $< > $@

tests/programs/%: CXXFLAGS += -I.
tests/programs/%: tests/programs/%.cpp $(RUNTIME)
	$(CXX) $(CXXFLAGS) $< $(LDLIBS) -o $@

# TODO

#INSTALL_DIR = /usr/local/bin

#install: lisp
#	cp lisp $(INSTALL_DIR)/

#uninstall:
#	rm $(INSTALL_DIR)/lisp
