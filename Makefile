
OS = $(shell uname -s)

target = bin/c2

.PHONY: dlls

all: $(target)
	@

SOURCES += $(wildcard src/*.cpp)

CFLAGS += -g -Idyncall/include -I../llvm-build/include -I../llvm/include -std=c++14 -arch x86_64 -Wno-switch -Wno-format-security

LLVM_LIBS += -lLLVMLTO -lLLVMPasses -lLLVMObjCARCOpts -lLLVMSymbolize -lLLVMDebugInfoPDB -lLLVMDebugInfoDWARF -lLLVMFuzzMutate -lLLVMTableGen -lLLVMDlltoolDriver -lLLVMLineEditor -lLLVMOrcJIT -lLLVMCoverage -lLLVMMIRParser -lLLVMTestingSupport -lLLVMObjectYAML -lLLVMLibDriver -lLLVMOption -lgtest_main -lgtest -lLLVMWindowsManifest -lLLVMX86Disassembler -lLLVMX86AsmParser -lLLVMX86CodeGen -lLLVMGlobalISel -lLLVMSelectionDAG -lLLVMAsmPrinter -lLLVMX86Desc -lLLVMMCDisassembler -lLLVMX86Info -lLLVMX86AsmPrinter -lLLVMX86Utils -lLLVMMCJIT -lLLVMInterpreter -lLLVMExecutionEngine -lLLVMRuntimeDyld -lLLVMCodeGen -lLLVMTarget -lLLVMCoroutines -lLLVMipo -lLLVMInstrumentation -lLLVMVectorize -lLLVMScalarOpts -lLLVMLinker -lLLVMIRReader -lLLVMAsmParser -lLLVMInstCombine -lLLVMBitWriter -lLLVMAggressiveInstCombine -lLLVMTransformUtils -lLLVMAnalysis -lLLVMProfileData -lLLVMObject -lLLVMMCParser -lLLVMMC -lLLVMDebugInfoCodeView -lLLVMDebugInfoMSF -lLLVMBitReader -lLLVMCore -lLLVMBinaryFormat -lLLVMSupport -lLLVMDemangle

LLVM_SYSTEM_LIBS += -lz -lcurses -lm -lxml2

LIBS += -lstdc++ -lm -ldl -lpthread -ldyncall_s -ldynload_s 
LIBS += $(LLVM_LIBS) $(LLVM_SYSTEM_LIBS)

ifeq ($(OS), Darwin)
  LFLAGS += -Ldyncall/lib/osx -L/Users/libarria/code/llvm-build/lib -Wl,-search_paths_first -Wl,-headerpad_max_install_names
  SOFLAGS += -dynamiclib
  SOEXT += dylib
endif
ifeq ($(OS), Linux)
  LFLAGS += -Ldyncall/lib/linux
  SOFLAGS += -shared
  SOEXT = so
endif

bin:
	mkdir -p bin

$(target): $(SOURCES) bin
	clang $(CFLAGS) $(SOURCES) -o bin/c2 -Ldyncall/lib $(LIBS) $(LFLAGS)

dynlibs = modules/Basic.$(SOEXT)

modules/Basic.$(SOEXT): modules/Basic.o
	clang $(SOFLAGS) -o modules/Basic.$(SOEXT) modules/Basic.o

modules/Basic.o: modules/Basic.cpp
	clang -g -fPIC -c modules/Basic.cpp -o modules/Basic.o

dlls: $(dynlibs)
	@

clean:
	rm bin/c2 modules/*.o modules/*.so
