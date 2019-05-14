#pragma once
#include "mytypes.h"

class Allocator
{
public:
	Allocator(u64 start_size = 0) {}
	virtual ~Allocator() {}
	virtual void* alloc(u64 size) = 0;
	virtual void free(void* p) = 0;
};

class PoolAllocator : public Allocator
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
    void * alloc(u64 size) override;
	void free(void* p) override;
    bool isAddressInRange(void *p);
};

void * operator new (u64 size, PoolAllocator *p);

class MallocAllocator : public Allocator
{
public:
	MallocAllocator() {}
	~MallocAllocator() {}

	void* alloc(u64 size) override;
	void free(void* p) override;
};

MallocAllocator* getMallocAllocator();