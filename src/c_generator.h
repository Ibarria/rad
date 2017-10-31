#pragma once
#include "AST.h"
#include <stdio.h>

class c_generator
{
    FILE *output_file;
    u32 ident;
    Array<VariableDeclarationAST *> dangling_functions;
    bool insert_dangling_funcs;
    void generate_preamble();
    void do_ident();
    void generate_line_info(BaseAST *ast);
    void generate_dangling_functions();
    void generate_function_prototype(VariableDeclarationAST *decl);
    void generate_variable_declaration(VariableDeclarationAST *decl);
    void generate_argument_declaration(ArgumentDeclarationAST *arg);
    void generate_statement_block(StatementBlockAST *block);
    void generate_statement(StatementAST *stmt);
    void generate_return_statement(ReturnStatementAST *ret);
    void generate_assignment(AssignmentAST *assign);
    void generate_expression(ExpressionAST *expr);
    void generate_function_call(FunctionCallAST *call);
    void generate_type(BaseAST *ast);
public:
    void generate_c_file(const char *filename, FileAST *root);
};

