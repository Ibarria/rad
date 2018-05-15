#pragma once
#include "AST.h"

void llvm_compile(FileAST *root, double &codegenTime, double &bingenTime, double &linkTime);

