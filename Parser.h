#pragma once

#include "AST.h"
#include "PoolAllocator.h"
#include "Lexer.h"
#include "Array.h"

struct Parser {
    Lexer *lex;
    char *errorString;
    Scope *current_scope;
    void Error(const char *msg);

    void MustMatchToken(TOKEN_TYPE type, char *msg = nullptr);
    void AddDeclarationToScope(DeclarationAST *decl);

    TypeAST *parseDirectType();
    TypeAST *parseType();
    ArgumentDeclarationAST *parseArgumentDeclaration();
    FunctionDeclarationAST *parseFunctionDeclaration();
    ReturnStatementAST *parseReturnStatement();
    StatementAST *parseStatement();
    StatementBlockAST *parseStatementBlock();
    FunctionDefinitionAST *parseFunctionDefinition();
    FunctionCallAST *parseFunctionCall();
    ExpressionAST * parseLiteral();
    ExpressionAST * parseUnaryExpression();
    ExpressionAST * parseBinOpExpressionRecursive(u32 oldprec, ExpressionAST *lhs);
    ExpressionAST * parseBinOpExpression();
    DefinitionAST * parseDefinition();
    DeclarationAST * parseDeclaration();
    ExpressionAST * parseAssignmentExpression();
    ExpressionAST * parseExpression();


    FileAST * Parse(const char *filename, PoolAllocator *pool);
};


