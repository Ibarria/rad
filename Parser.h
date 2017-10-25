#pragma once

#include "AST.h"
#include "PoolAllocator.h"
#include <vector>

std::vector<BaseAST *> Parse(const char *filename, PoolAllocator *pool);

