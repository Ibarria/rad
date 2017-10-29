#pragma once

#include "Array.h"
#include "mytypes.h"
#include "TokenType.h"
#include "TextType.h"

struct BaseAST;
struct TypeAST;
struct ExpressionAST;
struct VariableDeclarationAST;

struct Scope {
    Scope *parent;
    Array<VariableDeclarationAST *>decls;
};

enum BasicType {
    BASIC_TYPE_BOOL,
    BASIC_TYPE_STRING,
    BASIC_TYPE_S8, 
    BASIC_TYPE_S16, 
    BASIC_TYPE_S32, 
    BASIC_TYPE_S64,
    BASIC_TYPE_U8, 
    BASIC_TYPE_U16, 
    BASIC_TYPE_U32, 
    BASIC_TYPE_U64,
    BASIC_TYPE_F32, 
    BASIC_TYPE_F64
};

enum AST_CLASS_TYPE {
    AST_UNKNOWN,
    AST_FILE,
    AST_STATEMENT,
    AST_DEFINITION,
    AST_TYPE,
    AST_ARGUMENT_DECLARATION,
    AST_FUNCTION_TYPE,
    AST_STATEMENT_BLOCK,
    AST_RETURN_STATEMENT,
    AST_FUNCTION_DEFINITION,
    AST_EXPRESSION,
    AST_FUNCTION_CALL,
    AST_DIRECT_TYPE,
    AST_ARRAY_TYPE,
    AST_IDENTIFIER,
    AST_CONSTANT_NUMBER,
    AST_CONSTANT_STRING,
    AST_BINARY_OPERATION,
    AST_UNARY_OPERATION,
    AST_ASSIGNMENT,
    AST_VARIABLE_DECLARATION
};

struct BaseAST
{
    AST_CLASS_TYPE ast_type;
    BaseAST() { ast_type = AST_UNKNOWN; filename = nullptr; line_num = char_num = 0; scope = nullptr; }
    TextType filename;
    Scope *scope;
    u32 line_num;
    u32 char_num;
};

struct FileAST : BaseAST
{
    FileAST() { ast_type = AST_FILE; }
    Array<VariableDeclarationAST *>items;
    Scope scope;
};

struct StatementAST : BaseAST
{
};

struct DefinitionAST : StatementAST
{
    virtual bool needsSemiColon() const { return true; }
};

struct TypeAST : BaseAST
{
};

struct ArgumentDeclarationAST : BaseAST
{
    ArgumentDeclarationAST() { ast_type = AST_ARGUMENT_DECLARATION; name = nullptr; type = nullptr; }
    TextType name;
    TypeAST *type;
};

struct FunctionTypeAST : TypeAST
{
    FunctionTypeAST() { ast_type = AST_FUNCTION_TYPE; return_type = nullptr; }
    Array<ArgumentDeclarationAST *> arguments;
    TypeAST *return_type;
};

struct StatementBlockAST : StatementAST
{
    StatementBlockAST() { ast_type = AST_STATEMENT_BLOCK; }
    Array<StatementAST *> statements;
    Scope scope;
};

struct ReturnStatementAST: StatementAST
{
    ReturnStatementAST() { ast_type = AST_RETURN_STATEMENT; ret = nullptr; }
    ExpressionAST *ret;
};
struct FunctionDefinitionAST : DefinitionAST
{
    FunctionDefinitionAST() { ast_type = AST_FUNCTION_DEFINITION; declaration = nullptr; function_body = nullptr; }
    FunctionTypeAST *declaration;
    StatementBlockAST *function_body;
    virtual bool needsSemiColon() const { return false; }
};

struct ExpressionAST : DefinitionAST
{
};

struct FunctionCallAST : ExpressionAST
{
    FunctionCallAST() { ast_type = AST_FUNCTION_CALL; function_name = nullptr; }
    Array<ExpressionAST *>args;
    TextType function_name;
};

struct DirectTypeAST : TypeAST
{
    DirectTypeAST() { ast_type = AST_DIRECT_TYPE; isArray = isPointer = false; name = nullptr; }

	BasicType type;
	bool isArray;
	bool isPointer;
	TextType name;
};

struct ArrayTypeAST : TypeAST
{
    ArrayTypeAST() { ast_type = AST_ARRAY_TYPE; contained_type = nullptr; size = 0; isDynamic = false; }
    TypeAST *contained_type;
    u64 size;
    bool isDynamic;
};

struct IdentifierAST : ExpressionAST
{
    IdentifierAST() { ast_type = AST_IDENTIFIER; decl = nullptr; name = nullptr; }
    VariableDeclarationAST *decl;
    TextType name;
};

struct ConstantNumberAST : ExpressionAST
{
    ConstantNumberAST() { ast_type = AST_CONSTANT_NUMBER; pl.pu64 = 0; }
    union payload {
        u32 pu32;
        u64 pu64;
        s32 ps32;
        s64 ps64;
        f32 pf32;
        f64 pf64;
    } pl;
    BasicType type;
};

struct ConstantStringAST : ExpressionAST
{
    ConstantStringAST() { ast_type = AST_CONSTANT_STRING; str = nullptr; }
    TextType str;
};

struct BinaryOperationAST : ExpressionAST
{
    BinaryOperationAST() { ast_type = AST_BINARY_OPERATION; ; lhs = rhs = nullptr; op = TK_INVALID; }
    ExpressionAST *lhs;
    ExpressionAST *rhs;
    TOKEN_TYPE op;
};

struct UnaryOperationAST : ExpressionAST
{
    UnaryOperationAST() { ast_type = AST_UNARY_OPERATION; }
};

struct AssignmentAST : ExpressionAST
{
    AssignmentAST() { ast_type = AST_ASSIGNMENT; lhs = rhs = nullptr; op = TK_INVALID; }
    ExpressionAST *lhs;
    ExpressionAST *rhs;
    TOKEN_TYPE op;
};

#define DECL_FLAG_IS_CONSTANT          0x1
#define DECL_FLAG_HAS_BEEN_INFERRED    0x2
#define DECL_FLAG_HAS_BEEN_GENERATED   0x4

struct VariableDeclarationAST : StatementAST
{
    VariableDeclarationAST() 
    { 
        ast_type = AST_VARIABLE_DECLARATION; varname = nullptr; 
        specified_type = nullptr; definition = nullptr; flags = 0;
    }
    TextType varname;
	TypeAST *specified_type;
    DefinitionAST *definition;
    u32 flags;
};

void printAST(const BaseAST*ast, int ident);
const char *BasicTypeToStr(BasicType t);