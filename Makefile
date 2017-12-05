
OS = $(shell uname -s)

SOURCES += $(wildcard src/*.cpp)

CFLAGS += -g -Idyncall/include -std=c++14 -arch x86_64 -Wno-switch -Wno-format-security
LIBS += -lstdc++ -lm -ldl -lpthread -ldyncall_s -ldynload_s

ifeq ($(OS), Darwin)
  LFLAGS += -Ldyncall/lib/osx
endif
ifeq ($(OS), Linux)
  LFLAGS += -Ldyncall/lib/linux
endif

c2: $(SOURCES)
	clang $(CFLAGS) $(SOURCES) -o c2 -Ldyncall/lib $(LIBS) $(LFLAGS)

