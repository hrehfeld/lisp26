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

LDLIBS = -lSDL2

COMPILED_POSTFIX = .compiled
BIN = lisp lide
MOD = bignum builtin builtin_misc closure coerce cons cons_bind cons_impl core env env_install env_test error eval expr fixnum float gensym hash hash_bind hash_impl list meta number pointer printer reader reader_bind reader_test sdl2 spooky stream stream_impl string symbol system test time util vector
OBJ = $(MOD:%=%.o)
CPP = $(MOD:%=%.cpp)
COMPILED = hello comp blah comp-arith

RUNTIME = runtime.hpp lisp.hpp config.hpp font.hpp

RUNTIME_REQUIRED = lide hello blah comp

LINK.o = $(LINK.cc)

all: CXXFLAGS += -O2
all: executable

executable: $(BIN)

debug: CXXFLAGS += -g
debug: executable



clean:
	rm -rf $(BIN) $(OBJ)

# typing is hard
t: tests
e: extra
d: default-config
r: random-config

extra: $(COMPILED) blub.py render.cpp build.py 

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

lisp: $(OBJ)

%.o: %.cpp lisp.hpp config.hpp

stream.o: stream.cpp lisp.hpp config.hpp stream_impl.hpp

stream_impl.o: stream_impl.cpp lisp.hpp config.hpp stream_impl.hpp

$(RUNTIME_REQUIRED): $(RUNTIME)

# $(COMPILED): $(COMPILED:%=%$(COMPILED_POSTFIX).cpp)

comp: comp$(COMPILED_POSTFIX).cpp
hello: hello$(COMPILED_POSTFIX).cpp
blah: blah$(COMPILED_POSTFIX).cpp
comp-arith: comp-arith$(COMPILED_POSTFIX).cpp

%$(COMPILED_POSTFIX).cpp: %.lisp comp.lisp std.lisp lisp 
	./lisp load comp.lisp < $< > $@

%.py: %.lisp lisp2py.lisp lisp2x.lisp std.lisp lisp
	./lisp load lisp2py.lisp --no-comments $< > $@

render.cpp: render.lisp lisp2cpp.lisp lisp2x.lisp std.lisp lisp
	./lisp load lisp2cpp.lisp $< > $@

# TODO

#INSTALL_DIR = /usr/local/bin

#install: lisp
#	cp lisp $(INSTALL_DIR)/

#uninstall:
#	rm $(INSTALL_DIR)/lisp
