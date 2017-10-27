#pragma once
#include "AST.h"
#include <stdio.h>

class c_generator
{
    FILE *output_file;
    u32 ident;
    void generate_preamble();
    void do_ident();
    void generate_declaration(BaseAST *ast);
    void generate_type(BaseAST *ast);
public:
    c_generator();
    ~c_generator();
    void generate_c_file(const char *filename, FileAST *root);
};

