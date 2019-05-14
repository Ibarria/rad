#pragma once
#include "Allocator.h"

typedef char * TextType;

TextType CreateTextType(PoolAllocator *p, const char *src);

