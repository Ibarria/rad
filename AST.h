#pragma once

#define MAX_VARNAME 64

class BaseAST
{
public:
	BaseAST();
	~BaseAST();
	virtual void print() {}
};

class ExprAST : BaseAST
{
public:
	ExprAST();
	~ExprAST();
	virtual void print();
};

class TypeAST : BaseAST
{
public:
	TypeAST();
	~TypeAST();
	virtual void print();

	enum BasicType {
		I8, I16, I32, I64,
		U8, U16, U32, U64,
		F32, F64, BOOL, CUSTOM
	};

	BasicType type;
	bool isArray;
	bool isPointer;
	char name[MAX_VARNAME];
};


class ConstNumAST : ExprAST
{
public:
	ConstNumAST();
	~ConstNumAST();
	virtual void print();
};

class BinOpAST : ExprAST
{
public:
	BinOpAST();
	~BinOpAST();
	virtual void print();
};

class UnOpAST : ExprAST
{
public:
	UnOpAST();
	~UnOpAST();
	virtual void print();
};

class AssignAST : ExprAST
{
public:
	AssignAST();
	~AssignAST();
	virtual void print();
};

class CompareAST : ExprAST
{
public:
	CompareAST();
	~CompareAST();
	virtual void print();
};

class DeclAST : BaseAST
{
public:
	char varname[MAX_VARNAME];
	TypeAST type;

	DeclAST();
	~DeclAST();
	virtual void print();
};

