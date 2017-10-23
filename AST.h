#pragma once

#include <string>
#include <vector>
#include "mytypes.h"
#include "TokenType.h"

struct BaseAST;
struct TypeAST;
struct ExprAST;
struct DeclAST;

enum BasicType {
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64
};


struct BaseAST
{
	virtual void print(int ident) {}
};

struct StatementAST : BaseAST
{
    virtual void print(int ident) {}
};

struct DefinitionAST : StatementAST
{
    virtual bool needsSemiColon() { return true; }
    virtual void print(int ident) {}
};

struct TypeAST : BaseAST
{
    virtual void print(int ident) {}
};

struct ArgumentDeclarationAST : BaseAST
{
    std::string name;
    TypeAST *type;
    virtual void print(int ident);
};

struct FunctionDeclarationAST : TypeAST
{
    std::vector<ArgumentDeclarationAST *> arguments;
    TypeAST *return_type;
    virtual void print(int ident);
};

struct StatementBlockAST : StatementAST
{
    std::vector<StatementAST *> statements;
    virtual void print(int ident);
};

struct ReturnStatementAST: StatementAST
{
    ExprAST *ret;
    virtual void print(int ident);
};
struct FunctionDefinitionAST : DefinitionAST
{
    FunctionDeclarationAST *declaration;
    StatementBlockAST *function_body;
    virtual bool needsSemiColon() { return false; }
    virtual void print(int ident);
};

struct ExprAST : DefinitionAST
{
    virtual void print(int ident) {}
};

struct DirectTypeAST : TypeAST
{
	virtual void print(int ident);

	BasicType type;
    bool isString;
	bool isArray;
	bool isPointer;
	std::string name;
};

struct IdentAST : ExprAST
{
    DeclAST *decl;
    std::string name;
    virtual void print(int ident);
};

struct ConstNumAST : ExprAST
{
    union payload {
        u32 pu32;
        u64 pu64;
        s32 ps32;
        s64 ps64;
        f32 pf32;
        f64 pf64;
    } pl;
    BasicType type;
	virtual void print(int ident);
};

struct ConstStringAST : ExprAST
{
    std::string str;
    virtual void print(int ident);
};

struct BinOpAST : ExprAST
{
    ExprAST *lhs;
    ExprAST *rhs;
    TOKEN_TYPE op;
	virtual void print(int ident);
};

struct UnOpAST : ExprAST
{
	UnOpAST();
	~UnOpAST();
	virtual void print();
};

struct AssignAST : ExprAST
{
    ExprAST *lhs;
    ExprAST *rhs;
    TOKEN_TYPE op;
	virtual void print(int ident);
};

struct DeclAST : StatementAST
{
    std::string varname;
	TypeAST *specified_type;
    TypeAST *inferred_type;
    DefinitionAST *definition;
    bool is_constant;
	virtual void print(int ident);
};

