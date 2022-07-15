#pragma once

#include "AST.h"
#include "Allocator.h"
#include "Lexer.h"
#include "Array.h"

struct Interpreter;

struct Parser {
    Lexer *lex = nullptr;
    PoolAllocator *pool = nullptr;
    Interpreter *interp = nullptr;
    char errorStringBuffer[4096];
    char *errorString = nullptr;
    Scope *current_scope = nullptr;
    FileAST *top_level_ast = nullptr;
    bool isImport = false;
    bool success;
    bool isFunctionLevel = false; // true if the current parsing is a function body or directly. False for if, while
    void Error(const char *msg, ...);
    void ErrorWithLoc(SrcLocation &loc, const char *msg, ...);

    bool MustMatchToken(TOKEN_TYPE type, const char *msg = nullptr);
    bool AddDeclarationToScope(VariableDeclarationAST *decl);
    bool AddDeclarationToStruct(StructDefinitionAST *struct_def, VariableDeclarationAST *decl);

    void defineBuiltInTypes();

    DirectTypeAST *createType(TOKEN_TYPE tktype, TextType name);
    DirectTypeAST *getType(TOKEN_TYPE tktype, TextType name);
    TypeAST *parseDirectType();
    TypeAST *parseType();
    VariableDeclarationAST *parseArgumentDeclaration();
    FunctionTypeAST *parseFunctionDeclaration();
    ReturnStatementAST *parseReturnStatement();
    IfStatementAST *parseIfStatement();
    ForStatementAST *parseForStatement();
    StatementAST *parseStatement();
    StatementBlockAST *parseStatementBlock(FunctionDefinitionAST *fundef = nullptr);
    FunctionDefinitionAST *parseFunctionDefinition();
    StructDefinitionAST *parseStructDefinition();
    FunctionCallAST *parseFunctionCall();
    CastAST* parseCast();
    VarReferenceAST * parseVarReference();

    ExpressionAST * parseLiteral();
    ExpressionAST * parseUnaryExpression();
    ExpressionAST * parseBinOpExpressionRecursive(u32 oldprec, ExpressionAST *lhs);
    ExpressionAST * parseBinOpExpression();
    DefinitionAST * parseDefinition();
    VariableDeclarationAST * parseDeclaration(bool isStruct = false);
    ExpressionAST * parseAssignmentOrExpression();
    ExpressionAST * parseExpression();
    void parseImportDirective();
    void parseLoadDirective();
    RunDirectiveAST * parseRunDirective();
    FileAST * Parse(const char *filename, PoolAllocator *pool, FileAST *fast = nullptr);
    FileAST * ParseFromString(const char *str, u64 str_size, PoolAllocator *pool, FileAST *fast = nullptr);
    FileAST * ParseInternal(FileAST *fast);
};

