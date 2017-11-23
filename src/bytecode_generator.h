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
    s16 src_reg = -1;
    s16 src2_reg = -1;
    s16 dst_reg = -1;
    u8 op_size = 0;
};

union bc_register {
    u64 _u64 = 0;
    f64 _f64;
    u8 *_ptr;
};

struct bc_base_memory {
    u8 *mem = nullptr;
    u8 *stack_start = nullptr;
    u8 *stack_base = nullptr;
    u8 *stack_pointer = nullptr;
    u64 stack_size = 0; // debug information
    u64 alloc_size = 0; // debug information
    u64 used_size = 0; // debug information

    void initMem(u8 *basemem, u64 bss_size, u64 stack_size);
};

struct bytecode_function
{
	Array<BCI *> instructions;
	TextType function_name = nullptr;
    u64 bc_params_size = 0; // space to take from the stack for this function
	u64 function_id;
};

struct external_library
{
    TextType name = nullptr;
    void *dll = nullptr;
};

struct call_register {
    bc_register[128];
};

struct bytecode_machine
{
    bc_register regs[128]; // Maybe do SSA, like LLVM wants?
    s16 regs_used = 0;
    s16 reserve_register(u64 count = 1)
    {
        assert(regs_used + count < 128);
        s16 ret_value = regs_used;
        regs_used += (u16)count;
        return ret_value;
    }
    s16 reg_mark() const { return regs_used; }
    void pop_mark(s16 mark) { regs_used = mark; }
};

struct bytecode_program
{
    bc_base_memory bss;
    bytecode_machine machine;
	bytecode_function preamble_function;
	bytecode_function *start_function = nullptr;
    Array<bytecode_function *> functions;
    Array<external_library *>external_libs;
};

struct bytecode_generator
{
    PoolAllocator *pool = nullptr;
    bytecode_program *program = nullptr;
	bytecode_function *current_function = nullptr;

    external_library *findOrLoadLibrary(TextType filename);

    BCI *create_instruction(BytecodeInstructionOpcode opcode, s16 src_reg, s16 dst_reg, u64 big_const);
    void createStoreInstruction(VariableDeclarationAST *decl, s16 reg);
    void createStoreInstruction(BytecodeInstructionOpcode opcode, u64 bc_mem_offset, u64 size_in_bytes, s16 reg);
    void createLoadInstruction(VariableDeclarationAST *decl, s16 reg);
    void createLoadInstruction(BytecodeInstructionOpcode opcode, u64 bc_mem_offset, u64 size_in_bytes, s16 reg);
    void issue_instruction(BCI *bci);
    void issueReserveStackSpace(u64 size);

    void setPool(PoolAllocator *p) { pool = p; }
    bytecode_program *compileToBytecode(FileAST *root);

    void initializeVariablesInScope(Scope *scope);
    void initializeVariable(VariableDeclarationAST *decl);
    void generate_function(TextType name, FunctionDefinitionAST *fundef);
    void generate_statement_block(StatementBlockAST *block);

    void computeExpressionIntoRegister(ExpressionAST *expr, s16 reg);
    void compute_function_call_into_register(FunctionCallAST *funcall, s16 reg);
};

struct bytecode_runner
{
    bytecode_program *program = nullptr;
    call_register *current_call_register = nullptr;
    call_register *inner_call_register = nullptr;
    void *CallVM = nullptr;

    void run_bc_function(bytecode_function *func);

    void run_preamble();
    ExpressionAST * run_directive(RunDirectiveAST *run);
    void callExternalFunction(FunctionDefinitionAST *fundef, BCI *bci);
};

void print_bc_program(bytecode_program *program);
