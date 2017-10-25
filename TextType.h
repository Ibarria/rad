#pragma once
#include "PoolAllocator.h"

typedef char * TextType;

TextType CreateTextType(PoolAllocator *p, const char *src);
TextType CreateTextType(PoolAllocator *p, u64 size);

