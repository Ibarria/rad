#pragma once

#include "Array.h"
#include "mytypes.h"
#include "TokenType.h"
#include "TextType.h"

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
    const char *filename;
    unsigned int line_num;
    unsigned int char_num;
	virtual void print(int ident) const {}
};

struct StatementAST : BaseAST
{
    virtual void print(int ident) const {}
};

struct DefinitionAST : StatementAST
{
    virtual bool needsSemiColon() const { return true; }
    virtual void print(int ident) const {}
};

struct TypeAST : BaseAST
{
    virtual void print(int ident) const {}
};

struct ArgumentDeclarationAST : BaseAST
{
    TextType name;
    TypeAST *type;
    virtual void print(int ident) const;
};

struct FunctionDeclarationAST : TypeAST
{
    Array<ArgumentDeclarationAST *> arguments;
    TypeAST *return_type;
    virtual void print(int ident) const;
};

struct StatementBlockAST : StatementAST
{
    Array<StatementAST *> statements;
    virtual void print(int ident) const;
};

struct ReturnStatementAST: StatementAST
{
    ExprAST *ret;
    virtual void print(int ident) const;
};
struct FunctionDefinitionAST : DefinitionAST
{
    FunctionDeclarationAST *declaration;
    StatementBlockAST *function_body;
    virtual bool needsSemiColon() const { return false; }
    virtual void print(int ident) const;
};

struct ExprAST : DefinitionAST
{
    virtual void print(int ident) const {}
};

struct FunctionCallAST : ExprAST
{
    Array<StatementAST *>args;
    TextType function_name;
    virtual void print(int ident) const;
};

struct DirectTypeAST : TypeAST
{
	virtual void print(int ident) const;

	BasicType type;
    bool isString;
	bool isArray;
	bool isPointer;
	TextType name;
};

struct IdentAST : ExprAST
{
    DeclAST *decl;
    TextType name;
    virtual void print(int ident) const;
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
	virtual void print(int ident) const;
};

struct ConstStringAST : ExprAST
{
    TextType str;
    virtual void print(int ident) const;
};

struct BinOpAST : ExprAST
{
    ExprAST *lhs;
    ExprAST *rhs;
    TOKEN_TYPE op;
	virtual void print(int ident) const;
};

struct UnOpAST : ExprAST
{
	UnOpAST();
	~UnOpAST();
	virtual void print() const;
};

struct AssignAST : ExprAST
{
    ExprAST *lhs;
    ExprAST *rhs;
    TOKEN_TYPE op;
	virtual void print(int ident) const;
};

struct DeclAST : StatementAST
{
    TextType varname;
	TypeAST *specified_type;
    TypeAST *inferred_type;
    DefinitionAST *definition;
    bool is_constant;
	virtual void print(int ident) const;
};

