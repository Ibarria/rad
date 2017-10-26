#pragma once

#include "AST.h"
#include "PoolAllocator.h"
#include "Lexer.h"
#include "Array.h"

struct Parser {
    Lexer *lex;
    char *errorString;
    void Error(const char *msg);

    void MustMatchToken(TOKEN_TYPE type, char *msg = nullptr);

    TypeAST *parseDirectType();
    TypeAST *parseType();
    ArgumentDeclarationAST *parseArgumentDeclaration();
    FunctionDeclarationAST *parseFunctionDeclaration();
    ReturnStatementAST *parseReturnStatement();
    StatementAST *parseStatement();
    StatementBlockAST *parseStatementBlock();
    FunctionDefinitionAST *parseFunctionDefinition();
    FunctionCallAST *parseFunctionCall();
    ExprAST * parseCastExpression();
    ExprAST * parseBinOpExpressionRecursive(u32 oldprec, ExprAST *lhs);
    ExprAST * parseBinOpExpression();
    DefinitionAST * parseDefinition();
    DeclAST * parseDeclaration();
    ExprAST * parseAssignmentExpression();
    ExprAST * parseExpression();


    Array<BaseAST *> * Parse(const char *filename, PoolAllocator *pool);
};


