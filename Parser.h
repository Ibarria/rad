#pragma once

#include "AST.h"
#include <vector>

std::vector<BaseAST *> Parse(const char *filename);

