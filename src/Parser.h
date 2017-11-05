#pragma once

#include "AST.h"
#include "PoolAllocator.h"
#include "Lexer.h"
#include "Array.h"

struct Parser {
    Lexer *lex;
    PoolAllocator *pool = nullptr;
    char errorString[512];
    Scope *current_scope = nullptr;
    FileAST *top_level_ast = nullptr;
    bool isImport = false;
    bool success;
    void Error(const char *msg, ...);

    bool MustMatchToken(TOKEN_TYPE type, const char *msg = nullptr);
    bool AddDeclarationToScope(VariableDeclarationAST *decl);

    TypeAST *parseDirectType();
    TypeAST *parseType();
    ArgumentDeclarationAST *parseArgumentDeclaration();
    FunctionTypeAST *parseFunctionDeclaration();
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
    VariableDeclarationAST * parseDeclaration();
    ExpressionAST * parseAssignmentExpression();
    ExpressionAST * parseExpression();
    void parseImportDirective();
    void parseLoadDirective();
    RunDirectiveAST * parseRunDirective();
    FileAST * Parse(const char *filename, PoolAllocator *pool, FileAST *fast = nullptr);
};

void traverseAST(FileAST *root);

