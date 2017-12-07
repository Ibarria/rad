
OS = $(shell uname -s)

target = bin/c2

all: $(target)
	@

SOURCES += $(wildcard src/*.cpp)

CFLAGS += -g -Idyncall/include -std=c++14 -arch x86_64 -Wno-switch -Wno-format-security
LIBS += -lstdc++ -lm -ldl -lpthread -ldyncall_s -ldynload_s

ifeq ($(OS), Darwin)
  LFLAGS += -Ldyncall/lib/osx
endif
ifeq ($(OS), Linux)
  LFLAGS += -Ldyncall/lib/linux
endif

bin:
	mkdir -p bin

$(target): $(SOURCES) bin
	clang $(CFLAGS) $(SOURCES) -o bin/c2 -Ldyncall/lib $(LIBS) $(LFLAGS)

dynlibs = modules/Basic.so

modules/Basic.so: modules/Basic.o
	clang -shared -o modules/Basic.so modules/Basic.o

modules/Basic.o: modules/Basic.cpp
	clang -g -fPIC -c modules/Basic.cpp -o modules/Basic.o

dlls: $(dynlibs)
	@

clean:
	rm bin/c2 modules/*.o modules/*.so
