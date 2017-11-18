
SOURCES += $(filter-out src/windows%, $(wildcard src/*.cpp))

c2: $(SOURCES)
	clang -g -std=c++14 -arch x86_64 -Wno-switch -Wno-format-security $(SOURCES) -o c2 -lstdc++

