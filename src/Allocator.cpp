#include "Allocator.h"
#ifdef WIN32
# include <windows.h>
#endif
#include <stdio.h>
#include <stdlib.h>

#define MEGABYTES (1024*1024)

void * operator new (u64 size, PoolAllocator *p)
{
    return p->alloc(size);
}

void PoolAllocator::allocateBlock(block * b)
{
#ifdef WIN32	
    b->start_address = (u8 *)VirtualAlloc(NULL, block_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
#else
	b->start_address = (u8 *)malloc(block_size);
#endif		
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
#ifdef WIN32		
        VirtualFree(b->start_address, 0, MEM_RELEASE);
#else
		free(b->start_address);
#endif				
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

void PoolAllocator::free(void* p)
{
	// Do nothing, pool allocator frees it all on destruction
}

bool PoolAllocator::isAddressInRange(void * p)
{
    if ((p >= root_block.start_address) &&
        (p < root_block.free_address)) {
        return true;
    }
    return false;
}

void* MallocAllocator::alloc(u64 size) 
{
	return malloc(size);
}

void MallocAllocator::free(void* p)
{
	::free(p);
}

static MallocAllocator mAlloc;

MallocAllocator* getMallocAllocator()
{
	return &mAlloc;
}