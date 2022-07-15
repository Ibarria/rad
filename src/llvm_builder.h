#pragma once
#include "AST.h"

struct llvm_options {
	bool option_llvm_print = false;
    bool option_quiet = true;
	bool option_debug_info = false;
	bool option_optimize = false;
};

struct llvm_timing {
	double codegenTime = 0;
	double bingenTime = 0;
	double linkTime = 0;
};

void llvm_compile(FileAST *root, FileObject &obj_file, const llvm_options& opt, llvm_timing& timing, 
	 const char* output_name);

