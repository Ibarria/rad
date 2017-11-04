
SOURCES += $(filter-out src/windows%, $(wildcard src/*.cpp))

c2: $(SOURCES)
	clang -std=c++14 -Wno-switch -Wno-format-security $(SOURCES) -o c2 -lstdc++

