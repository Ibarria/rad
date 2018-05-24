#pragma once
#include "AST.h"

#if defined(_WIN32)
#define DLLEXPORT __declspec(dllexport)
#else 
#define DLLEXPORT 
#endif

extern "C" DLLEXPORT void llvm_compile(FileAST *root, double &codegenTime, double &bingenTime, double &linkTime, bool option_llvm_print);

