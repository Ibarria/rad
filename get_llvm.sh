#!/bin/bash
mkdir llvm
cd llvm
svn checkout -r 332840 http://llvm.org/svn/llvm-project/llvm/trunk llvm-src

cd llvm-src/tools
svn checkout -r 332840 http://llvm.org/svn/llvm-project/cfe/trunk clang
svn checkout -r 332840 http://llvm.org/svn/llvm-project/lld/trunk lld
svn checkout -r 332840 http://llvm.org/svn/llvm-project/lldb/trunk lldb

cd ../../
cd llvm-src/projects
svn checkout -r 332840 http://llvm.org/svn/llvm-project/libcxx/trunk libcxx
svn checkout -r 332840 http://llvm.org/svn/llvm-project/libcxxabi/trunk libcxxabi

cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS=lld -G "Unix Makefiles" -DLLVM_TARGETS_TO_BUILD="X86"

# you might need these packages in linux:
# libncurses-dev libxml2-dev python3-dev python3-pip python3-tk python3-lxml python3-six swig3.0 ninja-build