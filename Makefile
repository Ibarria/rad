
SOURCES += $(wildcard src/*.cpp)

CFLAGS += -g -Idyncall/include -std=c++14 -arch x86_64 -Wno-switch -Wno-format-security
LIBS += -lstdc++ -lm -ldl -lpthread -ldyncall_s -ldynload_s


c2: $(SOURCES)
	clang $(CFLAGS) $(SOURCES) -o c2 -Ldyncall/lib $(LIBS)

