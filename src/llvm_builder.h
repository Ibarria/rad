#pragma once
#include "AST.h"

void llvm_compile(FileAST *root, FileObject &obj_file, double &codegenTime, double &bingenTime, double &linkTime, 
	bool option_llvm_print, bool option_quiet, bool option_debug_info, const char* output_name);

