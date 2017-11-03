#pragma once
#include "AST.h"


struct Interpreter
{   
    PoolAllocator pool;
    Scope *current_scope; // current scope
    bool success = true;
    Array<TextType> errors;
    char errorString[512];
    void Error(BaseAST *ast, const char *msg, ...);

    bool isConstExpression(ExpressionAST *expr);
    bool checkVariablesInExpression(ExpressionAST *expr);
    TypeAST * deduceType(ExpressionAST *expr);

    bool infer_types(VariableDeclarationAST *decl);

    u32 process_scope_variables(Scope * scope);
    void process_all_scope_variables(Scope *scope);

    void traverseAST(FileAST *root);
    void traverseAST(StatementBlockAST *root);
    void printErrors();
};

