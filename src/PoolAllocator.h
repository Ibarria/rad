#pragma once
#include "mytypes.h"

class PoolAllocator
{
    struct block {
        u64 free_size;
        struct block *next;
        u8 *start_address;
        u8 *free_address;
    };
    block root_block;
    u64 total_size;
    u64 block_size;

    void allocateBlock(block *b);
    void * allocateFromBlock(block *b, u64 size);
public:
    PoolAllocator(u64 start_size = 0);
    ~PoolAllocator();
    void * alloc(u64 size);
};

void * operator new (u64 size, PoolAllocator *p);