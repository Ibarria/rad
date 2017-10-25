#include "TextType.h"
#include <string.h>

TextType CreateTextType(PoolAllocator * p, const char * src)
{
    u64 size = strlen(src) + 1;
    TextType text = (TextType)p->alloc(size);
    strncpy_s(text, size, src, size);
    return text;
}

TextType CreateTextType(PoolAllocator * p, u64 size)
{
    TextType text = (TextType)p->alloc(size);
    return text;
}
