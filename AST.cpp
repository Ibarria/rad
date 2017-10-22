#include "AST.h"

BaseAST::BaseAST()
{
}

BaseAST::~BaseAST()
{
}

ExprAST::ExprAST()
{
}

ExprAST::~ExprAST()
{
}

void ExprAST::print()
{
}

ConstNumAST::ConstNumAST()
{
}

ConstNumAST::~ConstNumAST()
{
}

void ConstNumAST::print()
{
}

BinOpAST::BinOpAST()
{
}

BinOpAST::~BinOpAST()
{
}

void BinOpAST::print()
{
}

UnOpAST::UnOpAST()
{
}

UnOpAST::~UnOpAST()
{
}

void UnOpAST::print()
{
}

AssignAST::AssignAST()
{
}

AssignAST::~AssignAST()
{
}

void AssignAST::print()
{
}

CompareAST::CompareAST()
{
}

CompareAST::~CompareAST()
{
}

void CompareAST::print()
{
}

DeclAST::DeclAST()
{
    specified_type = inferred_type = nullptr;
}

DeclAST::~DeclAST()
{
}

void DeclAST::print()
{
}

void DirectTypeAST::print()
{
}
