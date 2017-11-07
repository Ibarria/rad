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
    s16 src_reg;
    s16 dst_reg;
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
    bc_register regs[64];
    s16 regs_used = 0;
    Array<BCI *> instructions;
    s16 reserve_register() { assert(regs_used < 64);  return regs_used++; }
    s16 reg_mark() const { return regs_used; }
    void pop_mark(s16 mark) { regs_used = mark; }
};

struct bytecode_generator
{
    PoolAllocator *pool = nullptr;
    bytecode_program *program = nullptr;

    BCI *create_instruction(BytecodeInstructionOpcode opcode, s16 src_reg, s16 dst_reg, u64 big_const);
    void issue_instruction(BCI *bci);

    void setPool(PoolAllocator *p) { pool = p; }
    bytecode_program *compileToBytecode(FileAST *root);

    void initializeVariablesInScope(Scope *scope);
    void initializeVariable(VariableDeclarationAST *decl);

    void computeExpressionIntoRegister(ExpressionAST *expr, s16 reg);
};

