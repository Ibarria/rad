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
    BaseAST() { ast_type = AST_UNKNOWN; }
    TextType filename = nullptr;
    Scope *scope = nullptr;
    u32 line_num = 0;
    u32 char_num = 0;
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
    ArgumentDeclarationAST() { ast_type = AST_ARGUMENT_DECLARATION; }
    TextType name = nullptr;
    TypeAST *type = nullptr;
};

struct FunctionTypeAST : TypeAST
{
    FunctionTypeAST() { ast_type = AST_FUNCTION_TYPE; }
    Array<ArgumentDeclarationAST *> arguments;
    TypeAST *return_type = nullptr;
};

struct StatementBlockAST : StatementAST
{
    StatementBlockAST() { ast_type = AST_STATEMENT_BLOCK; }
    Array<StatementAST *> statements;
    Scope scope;
};

struct ReturnStatementAST: StatementAST
{
    ReturnStatementAST() { ast_type = AST_RETURN_STATEMENT; }
    ExpressionAST *ret = nullptr;
};
struct FunctionDefinitionAST : DefinitionAST
{
    FunctionDefinitionAST() { ast_type = AST_FUNCTION_DEFINITION; }
    FunctionTypeAST *declaration = nullptr;
    StatementBlockAST *function_body = nullptr;
    virtual bool needsSemiColon() const { return false; }
};

struct ExpressionAST : DefinitionAST
{
};

struct FunctionCallAST : ExpressionAST
{
    FunctionCallAST() { ast_type = AST_FUNCTION_CALL; }
    Array<ExpressionAST *>args;
    TextType function_name = nullptr;
};

struct DirectTypeAST : TypeAST
{
    DirectTypeAST() { ast_type = AST_DIRECT_TYPE; }

	BasicType type;
	bool isArray = false;
	bool isPointer = false;
	TextType name = nullptr;
};

struct ArrayTypeAST : TypeAST
{
    ArrayTypeAST() { ast_type = AST_ARRAY_TYPE; }
    TypeAST *contained_type = nullptr;
    u64 num_elems = 0;
    bool isDynamic = false;
};

struct IdentifierAST : ExpressionAST
{
    IdentifierAST() { ast_type = AST_IDENTIFIER; }
    VariableDeclarationAST *decl = nullptr;
    TextType name = nullptr;
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
    ConstantStringAST() { ast_type = AST_CONSTANT_STRING; }
    TextType str = nullptr;
};

struct BinaryOperationAST : ExpressionAST
{
    BinaryOperationAST() { ast_type = AST_BINARY_OPERATION; }
    ExpressionAST *lhs = nullptr;
    ExpressionAST *rhs = nullptr;
    TOKEN_TYPE op = TK_INVALID;
};

struct UnaryOperationAST : ExpressionAST
{
    UnaryOperationAST() { ast_type = AST_UNARY_OPERATION; }
};

struct AssignmentAST : ExpressionAST
{
    AssignmentAST() { ast_type = AST_ASSIGNMENT; }
    ExpressionAST *lhs = nullptr;
    ExpressionAST *rhs = nullptr;
    TOKEN_TYPE op = TK_INVALID;
};

#define DECL_FLAG_IS_CONSTANT          0x1
#define DECL_FLAG_HAS_BEEN_INFERRED    0x2
#define DECL_FLAG_HAS_BEEN_GENERATED   0x4

struct VariableDeclarationAST : StatementAST
{
    VariableDeclarationAST() { ast_type = AST_VARIABLE_DECLARATION; }
    TextType varname = nullptr;
	TypeAST *specified_type = nullptr;
    DefinitionAST *definition = nullptr;
    u32 flags = 0;
};

void printAST(const BaseAST*ast, int ident);
const char *BasicTypeToStr(BasicType t);