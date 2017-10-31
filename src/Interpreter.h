#pragma once
#include "AST.h"


class Interpreter
{
public:
    Interpreter();
    ~Interpreter();
    void AnalyzeAST(FileAST *program);
};

