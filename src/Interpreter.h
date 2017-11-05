#pragma once
#include "AST.h"


struct Interpreter
{   
    PoolAllocator pool;
    Scope *current_scope; // current scope
    bool success = true;
    TextType current_identifier;
    Array<TextType> errors;
    char errorString[512];
    void Error(BaseAST *ast, const char *msg, ...);

    VariableDeclarationAST *validateVariable(IdentifierAST *a);
    VariableDeclarationAST *validateFunctionCall(FunctionCallAST *a);

    bool isConstExpression(ExpressionAST *expr);
    bool checkVariablesInExpression(ExpressionAST *expr);
    TypeAST * deduceType(ExpressionAST *expr);
    bool compatibleTypes(TypeAST *lhs, TypeAST *rhs);
    bool infer_types(VariableDeclarationAST *decl);

    u32 process_scope_variables(Scope * scope);
    void process_all_scope_variables(Scope *scope);

    void traverseAST(FileAST *root);
    void traverseAST(StatementBlockAST *root);
    void traverseAST(ExpressionAST *root);
    void printErrors();
};

