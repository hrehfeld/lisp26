
BIN = lisp lide
MOD = bignum builtin builtin_misc closure coerce cons cons_bind cons_impl core env env_install env_test error eval expr fixnum float gensym hash hash_bind hash_impl list meta number pointer printer reader reader_bind reader_test sdl2 spooky stream stream_impl string symbol system test time util vector
OBJ = $(MOD:%=%.o)
CPP = $(MOD:%=%.cpp)

RUNTIME = runtime.hpp lisp.hpp config.hpp font.hpp

all: $(BIN)

clean:
	rm -rf $(BIN) $(OBJ)

# typing is hard
t: tests
e: extra
d: default-config
r: random-config

extra: hello blah comp blub.py render.cpp build.py

tests: lisp std.lisp std.test
	./lisp test
	./lisp test core.test
	./lisp test std.lisp std.test
	./lisp test meta.lisp meta.test
	./lisp test hash.lisp hash.test

default-config:
	python3 fuzz.py default

random-config:
	python3 fuzz.py random

%.o: %.cpp lisp.hpp config.hpp
	g++ -c -std=c++11 -Wall -O2 $< -o $@

stream.o: stream.cpp lisp.hpp config.hpp stream_impl.hpp
	g++ -c -std=c++11 -Wall -O2 $< -o $@

stream_impl.o: stream_impl.cpp lisp.hpp config.hpp stream_impl.hpp
	g++ -c -std=c++11 -Wall -O2 $< -o $@

lisp: $(OBJ) lisp.o
	g++ -Wall -O2 $^ -lSDL2 -o $@

lide: lide.cpp $(RUNTIME)
	g++ -std=c++11 -Wall -O2 $< -lSDL2 -o $@

hello.cpp: hello.lisp comp.lisp std.lisp lisp
	./lisp load comp.lisp < $< > $@

hello: hello.cpp $(RUNTIME)
	g++ -std=c++11 -Wall -O2 $< -lSDL2 -o $@

comp.cpp: comp.lisp std.lisp lisp
	./lisp load comp.lisp < $< > $@

comp: comp.cpp $(RUNTIME)
	g++ -std=c++11 -Wall -O2 $< -lSDL2 -o $@

blah.cpp: blah.lisp comp.lisp std.lisp lisp
	./lisp load comp.lisp < $< > $@

blah: blah.cpp $(RUNTIME)
	g++ -std=c++11 -Wall -O2 $< -lSDL2 -o $@

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