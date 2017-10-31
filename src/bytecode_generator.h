#pragma once
#include "mytypes.h"
#include "Array.h"

struct bc_register {
    u64 _u64 = 0;
    f64 _f64 = 0;
    u8 *_ptr = nullptr;
};

struct bc_calling_record {
    bc_register regs[64];
    u8 stack[4096];
};

struct bc_base_memory {
    u8 *mem;
    u64 alloc_size;
    u64 used_size;
};

class bytecode_generator
{
public:
    bytecode_generator();
    ~bytecode_generator();
};

