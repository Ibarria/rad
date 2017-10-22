#pragma once

#include <string>
#include <vector>

struct BaseAST;
struct TypeAST;
struct ExprAST;

struct BaseAST
{
	BaseAST();
	~BaseAST();
	virtual void print() {}
};

struct DefinitionAST : BaseAST
{
    virtual void print() {}
};

struct TypeAST : BaseAST
{
    virtual void print() {}
};

struct ArgumentDeclarationAST : BaseAST
{
    std::string name;
    TypeAST *type;
    virtual void print();
};

struct FunctionDeclarationAST : TypeAST
{
    std::vector<ArgumentDeclarationAST *> arguments;
    TypeAST *return_value;
    virtual void print();
};

struct StatementBlockAST : BaseAST
{
    virtual void print();
};

struct FunctionDefinitionAST : DefinitionAST
{
    FunctionDeclarationAST *declaration;
    StatementBlockAST *function_body;
    virtual void print();
};

struct ExprAST : DefinitionAST
{
	ExprAST();
	~ExprAST();
	virtual void print();
};

struct DirectTypeAST : TypeAST
{
	virtual void print();

	enum BasicType {
		I8, I16, I32, I64,
		U8, U16, U32, U64,
		F32, F64, BOOL, STRING, CUSTOM
	};

	BasicType type;
	bool isArray;
	bool isPointer;
	std::string name;
};


struct ConstNumAST : ExprAST
{
	ConstNumAST();
	~ConstNumAST();
	virtual void print();
};

struct BinOpAST : ExprAST
{
	BinOpAST();
	~BinOpAST();
	virtual void print();
};

struct UnOpAST : ExprAST
{
	UnOpAST();
	~UnOpAST();
	virtual void print();
};

struct AssignAST : ExprAST
{
	AssignAST();
	~AssignAST();
	virtual void print();
};

struct CompareAST : ExprAST
{
	CompareAST();
	~CompareAST();
	virtual void print();
};

struct DeclAST : BaseAST
{
    std::string varname;
	TypeAST *specified_type;
    TypeAST *inferred_type;
    DefinitionAST *definition;
    bool is_constant;
	DeclAST();
	~DeclAST();
	virtual void print();
};

