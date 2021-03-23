#pragma once
#include "AST.h"

void llvm_compile(FileAST *root, FileObject &obj_file, double &codegenTime, double &bingenTime, double &linkTime, 
	bool option_llvm_print, bool option_quiet, const char* output_name);

