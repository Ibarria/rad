#include "PoolAllocator.h"
#include <windows.h>
#include <stdio.h>

#define MEGABYTES (1024*1024)

void PoolAllocator::allocateBlock(block * b)
{
    b->start_address = (u8 *)VirtualAlloc(NULL, block_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
    b->free_address = root_block.start_address;
    b->free_size = block_size;
    b->next = nullptr;

    total_size += block_size;
}

void * PoolAllocator::allocateFromBlock(block * b, u64 size)
{
    if (b->free_size >= size) {
        u8 *p = b->free_address;
        b->free_address += size;
        b->free_size -= size;
        return p;
    }
    return nullptr;
}

PoolAllocator::PoolAllocator(u64 start_size)
{
    if (start_size == 0) {
        block_size = 64 * MEGABYTES;
    } else {
        // Align to the megabyte
        start_size = (start_size >> 10) << 10;
        block_size = (start_size > 1 * MEGABYTES ? start_size : 1 * MEGABYTES);
    }
    total_size = 0;

    allocateBlock(&root_block);
}

PoolAllocator::~PoolAllocator()
{
    block *b, *f;
    for (b = &root_block; b != nullptr; ) {
        VirtualFree(b->start_address, 0, MEM_RELEASE);
        f = b;
        b = b->next;
        if (f != &root_block) {
            delete f;
        }
    }
}

void * PoolAllocator::alloc(u64 size)
{
    if (size > block_size) {
        printf("The allocator cannot handle such a large memory block!\n");
        return nullptr;
    }
    // find the first block with enough space for size
    block *b;
    void *p;
    for (b = &root_block; b != nullptr; b = b->next) {
        if ((p = allocateFromBlock(b, size)) != nullptr) {
            return p;
        }
    }

    // we need a new block
    b = new block();
    allocateBlock(b);
    b->next = root_block.next;
    root_block.next = b;
    return allocateFromBlock(b, size);
}
