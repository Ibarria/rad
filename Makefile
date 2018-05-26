
OS = $(shell uname -s)

target = bin/c2

.PHONY: dlls

all: $(target)
	@

SOURCES += $(filter-out $(wildcard src/llvm*), $(wildcard src/*.cpp))
LLVM_SOURCES += src/llvm_builder.cpp llvm_backend/llvm_backend.cpp src/Timer.cpp
LLVM_INCLUDES += -I../llvm-build/include -I../llvm/include -Isrc
DYNCALL_INCLUDES += -Idyncall/include

CFLAGS += -g -std=c++14 -arch x86_64 -Wno-switch -Wno-format-security

LLVM_LIBS += -lLLVMLTO -lLLVMPasses -lLLVMObjCARCOpts -lLLVMSymbolize -lLLVMDebugInfoPDB -lLLVMDebugInfoDWARF -lLLVMFuzzMutate -lLLVMTableGen -lLLVMDlltoolDriver -lLLVMLineEditor -lLLVMOrcJIT -lLLVMCoverage -lLLVMMIRParser -lLLVMTestingSupport -lLLVMObjectYAML -lLLVMLibDriver -lLLVMOption -lgtest_main -lgtest -lLLVMWindowsManifest -lLLVMX86Disassembler -lLLVMX86AsmParser -lLLVMX86CodeGen -lLLVMGlobalISel -lLLVMSelectionDAG -lLLVMAsmPrinter -lLLVMX86Desc -lLLVMMCDisassembler -lLLVMX86Info -lLLVMX86AsmPrinter -lLLVMX86Utils -lLLVMMCJIT -lLLVMInterpreter -lLLVMExecutionEngine -lLLVMRuntimeDyld -lLLVMCodeGen -lLLVMTarget -lLLVMCoroutines -lLLVMipo -lLLVMInstrumentation -lLLVMVectorize -lLLVMScalarOpts -lLLVMLinker -lLLVMIRReader -lLLVMAsmParser -lLLVMInstCombine -lLLVMBitWriter -lLLVMAggressiveInstCombine -lLLVMTransformUtils -lLLVMAnalysis -lLLVMProfileData -lLLVMObject -lLLVMMCParser -lLLVMMC -lLLVMDebugInfoCodeView -lLLVMDebugInfoMSF -lLLVMBitReader -lLLVMCore -lLLVMBinaryFormat -lLLVMSupport -lLLVMDemangle

LLVM_SYSTEM_LIBS += -lz -lcurses -lm -lxml2
DYNCALL_LIBS += -ldyncall_s -ldynload_s 
LIBS += -lstdc++ -lm -ldl -lpthread  

LLVM_LFLAGS += $(LIBS) $(LLVM_LIBS) $(LLVM_SYSTEM_LIBS)

ifeq ($(OS), Darwin)
  LFLAGS += -Lbin -Ldyncall/lib/osx bin/$(LLVM_LIB)
  LLVM_LFLAGS += -L/Users/libarria/code/llvm-build/lib -Wl,-search_paths_first -Wl,-headerpad_max_install_names
  SOFLAGS += -dynamiclib
  SOEXT += dylib
endif
ifeq ($(OS), Linux)
  LFLAGS += -Lbin -Ldyncall/lib/linux
  SOFLAGS += -shared
  SOEXT = so
endif

LLVM_LIB = llvm_backend.$(SOEXT)

bin/$(LLVM_LIB):
	clang $(CFLAGS) $(LLVM_INCLUDES) $(SOFLAGS) -g -fPIC $(LLVM_SOURCES) $(LLVM_LFLAGS) -o bin/$(LLVM_LIB) 

bin:
	mkdir -p bin

$(target): $(SOURCES) bin bin/$(LLVM_LIB)
	clang $(CFLAGS) $(DYNCALL_INCLUDES) $(SOURCES) -o bin/c2 $(LFLAGS) $(LIBS) $(DYNCALL_LIBS) 

dynlibs = modules/Basic.$(SOEXT)

modules/Basic.$(SOEXT): modules/Basic.o
	clang $(SOFLAGS) -o modules/Basic.$(SOEXT) modules/Basic.o

modules/Basic.o: modules/Basic.cpp
	clang -g -fPIC -c modules/Basic.cpp -o modules/Basic.o

dlls: $(dynlibs)
	@

clean:
	rm -f bin/c2 modules/*.o modules/*.so modules/*.dylib bin/*.dylib
	rm -fR c2.dSYM
