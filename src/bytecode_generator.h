#pragma once
#include "mytypes.h"
#include "Array.h"
#include "PoolAllocator.h"
#include "bytecode_instructions.h"
#include "AST.h"

// ByteCode Instruction
struct BCI {
    u64 big_const = 0;
    BytecodeInstructionOpcode opcode = BC_UNINITIALIZED;
    u32 inst_index = 0; // to be used as IP reg
    u16 src_reg;
    u16 dst_reg;
    u8 op_size;
};

union bc_register {
    u64 _u64 = 0;
    f64 _f64;
    u8 *_ptr;
};

struct bc_calling_record {
    bc_register regs[32];
};

struct bc_base_memory {
    u8 *mem = nullptr;
    u8 *stack_base = nullptr;
    u64 stack_index = 0;
    u64 stack_size = 0;
    u64 alloc_size = 0; // debug information
    u64 used_size = 0; // debug information

    void initMem(u8 *basemem, u64 bss_size, u64 stack_size);
};

struct bytecode_program
{
    bc_base_memory bss;

    Array<BCI *> instructions;

};

struct bytecode_generator
{
    PoolAllocator *pool = nullptr;
    bytecode_program *program = nullptr;

    void setPool(PoolAllocator *p) { pool = p; }
    bytecode_program *compileToBytecode(FileAST *root);


};

