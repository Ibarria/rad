#include "c_generator.h"
#include <stdio.h>
#include <stdlib.h>


void c_generator::generate_preamble()
{
    fprintf(output_file, "#include <stdio.h>\n");
    fprintf(output_file, "#include <stdlib.h>\n\n");
    fprintf(output_file, "typedef signed char         s8;\n");
    fprintf(output_file, "typedef signed short       s16;\n");
    fprintf(output_file, "typedef int                s32;\n");
    fprintf(output_file, "typedef long long          s64;\n");
    fprintf(output_file, "typedef unsigned char       u8;\n");
    fprintf(output_file, "typedef unsigned short     u16;\n");
    fprintf(output_file, "typedef unsigned int       u32;\n");
    fprintf(output_file, "typedef unsigned long long u64;\n\n");
    fprintf(output_file, " \n");
}

void c_generator::do_ident()
{
    if (ident > 0) fprintf(output_file, "%*s", ident, "");
}

void c_generator::generate_declaration(BaseAST * ast)
{
    assert(ast->ast_type = AST_DECLARATION);
    DeclarationAST *decl = (DeclarationAST *)ast;

    do_ident();

    
}

void c_generator::generate_type(BaseAST * ast)
{
}

c_generator::c_generator()
{
}


c_generator::~c_generator()
{
}

void c_generator::generate_c_file(const char * filename, FileAST * root)
{
    output_file = fopen(filename, "w");
    ident = 0;
    generate_preamble();

    fclose(output_file);
    output_file = nullptr;
}
