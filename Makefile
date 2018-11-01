
OS = $(shell uname -s)

target = bin/rad
root_dir := ../
root_dir := $(abspath $(root_dir))

.PHONY: dlls

all: $(target)
	@

SOURCES += $(wildcard src/*.cpp)
LLVM_SOURCES += src/llvm_builder.cpp llvm_backend/llvm_backend.cpp src/Timer.cpp
LLVM_INCLUDES += -I$(root_dir)/llvm/debug/include -I$(root_dir)/llvm/llvm-src/include -Isrc
DYNCALL_INCLUDES += -Idyncall/include

LLVM_ALL_FLAGS=$(shell ../llvm/debug/bin/llvm-config --cxxflags --ldflags --system-libs --libs all)

CFLAGS += -g -std=c++14 -Wno-switch -Wno-format-security

LLVM_LIBS += -lLLVMLTO -lLLVMPasses -lLLVMObjCARCOpts -lLLVMSymbolize -lLLVMDebugInfoPDB -lLLVMDebugInfoDWARF -lLLVMFuzzMutate -lLLVMTableGen -lLLVMDlltoolDriver -lLLVMLineEditor -lLLVMOrcJIT -lLLVMCoverage -lLLVMMIRParser -lLLVMTestingSupport -lLLVMObjectYAML -lLLVMLibDriver -lLLVMOption -lgtest_main -lgtest -lLLVMWindowsManifest -lLLVMX86Disassembler -lLLVMX86AsmParser -lLLVMX86CodeGen -lLLVMGlobalISel -lLLVMSelectionDAG -lLLVMAsmPrinter -lLLVMX86Desc -lLLVMMCDisassembler -lLLVMX86Info -lLLVMX86AsmPrinter -lLLVMX86Utils -lLLVMMCJIT -lLLVMInterpreter -lLLVMExecutionEngine -lLLVMRuntimeDyld -lLLVMCodeGen -lLLVMTarget -lLLVMCoroutines -lLLVMipo -lLLVMInstrumentation -lLLVMVectorize -lLLVMScalarOpts -lLLVMLinker -lLLVMIRReader -lLLVMAsmParser -lLLVMInstCombine -lLLVMBitWriter -lLLVMAggressiveInstCombine -lLLVMTransformUtils -lLLVMAnalysis -lLLVMProfileData -lLLVMObject -lLLVMMCParser -lLLVMMC -lLLVMDebugInfoCodeView -lLLVMDebugInfoMSF -lLLVMBitReader -lLLVMCore -lLLVMBinaryFormat -lLLVMSupport -lLLVMDemangle

LLVM_SYSTEM_LIBS += -lz -lcurses -lm -lxml2
DYNCALL_LIBS += -ldyncall_s -ldynload_s 
LIBS += -lstdc++ -lz -lcurses -lxml2 -lm -ldl -lpthread  

LLVM_LFLAGS += $(LIBS) $(LLVM_LIBS) $(LLVM_SYSTEM_LIBS)

ifeq ($(OS), Darwin)
  LFLAGS += -Lbin -Ldyncall/lib/osx bin/$(LLVM_LIB)
  LLVM_LFLAGS += -L$(root_dir)/llvm-build/lib -Wl,-search_paths_first -Wl,-headerpad_max_install_names
  SOFLAGS += -dynamiclib
  SOEXT += dylib
endif
ifeq ($(OS), Linux)
  LFLAGS += -Lbin -Ldyncall/lib/linux -L$(root_dir)/llvm/debug/lib 
# -Wl,-search_paths_first -Wl,-headerpad_max_install_names
  SOFLAGS += -shared
  SOEXT = so
endif

LLVM_LIB = llvm_backend.$(SOEXT)

bin/$(LLVM_LIB): $(LLVM_SOURCES)
	g++ $(CFLAGS) $(LLVM_INCLUDES) $(SOFLAGS) -g -fPIC $(LLVM_SOURCES) $(LLVM_LFLAGS) -o bin/$(LLVM_LIB) 

bin:
	mkdir -p bin

$(target): $(SOURCES) bin 
	g++ $(CFLAGS) $(DYNCALL_INCLUDES) $(LLVM_INCLUDES) $(SOURCES) -o $(target) $(LFLAGS) $(LIBS) $(LLVM_ALL_FLAGS) $(DYNCALL_LIBS) 
#	clang $(CFLAGS) $(DYNCALL_INCLUDES) $(LLVM_INCLUDES) $(SOURCES) -o $(target) $(LFLAGS) $(LIBS) $(LLVM_LIBS) $(DYNCALL_LIBS) 

dynlibs = modules/Basic.$(SOEXT)

modules/Basic.$(SOEXT): modules/Basic.o
	g++ $(SOFLAGS) -o modules/Basic.$(SOEXT) modules/Basic.o

modules/Basic.o: modules/Basic.cpp
	g++ -g -fPIC -c modules/Basic.cpp -o modules/Basic.o

dlls: $(dynlibs)
	@

clean:
	rm -f bin/rad modules/*.o modules/*.so modules/*.dylib bin/*.dylib
	rm -fR rad.dSYM
