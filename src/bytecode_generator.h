#pragma once
#include "mytypes.h"
#include "Array.h"
#include "PoolAllocator.h"
#include "bytecode_instructions.h"
#include "AST.h"

struct Interpreter;

union bc_register_data {
    u64 _u64 = 0;
    u32 _u32;
    u16 _u16;
     u8  _u8;
    s64 _s64;
    s32 _s32;
    s16 _s16;
     s8  _s8;
    f64 _f64;
    f32 _f32;
    u8 *_ptr; // this is really void, but keeping u8 for now
};

enum RegisterType : u8 {
    REGTYPE_UNKNOWN,
    REGTYPE_UINT, // bool are implemented as a u8, at this level
    REGTYPE_SINT,
    REGTYPE_FLOAT,
    REGTYPE_POINTER, 
};

struct bc_register {
    bc_register_data data;
    RegisterType type = REGTYPE_UNKNOWN;
    u8 bytes = 0; // from 1 through 8 only!
};

// ByteCode Instruction
struct BCI {
    u64 big_const = 0;
    BytecodeInstructionOpcode opcode = BC_UNINITIALIZED;
    s16 src_reg = -1;
    s16 src2_reg = -1;
    s16 dst_reg = -1;
    u8 dst_type_bytes = 0;
    s32 inst_index = -1; // This holds the relative position of the instruction on the function
    RegisterType dst_type = REGTYPE_UNKNOWN;
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
    Interpreter *interp;

    external_library *findOrLoadLibrary(TextType filename);

    BCI *create_instruction(BytecodeInstructionOpcode opcode, s16 src_reg, s16 dst_reg, u64 big_const);
    void createStoreInstruction(VariableDeclarationAST *decl, s16 reg);
    void createStoreInstruction(BytecodeInstructionOpcode opcode, u64 bc_mem_offset, u64 size_in_bytes, s16 reg, RegisterType regtype);
    void createStoreInstruction(BytecodeInstructionOpcode opcode, s16 ptrreg, s16 datareg, u64 size_in_bytes);
    void createLoadInstruction(VariableDeclarationAST *decl, s16 reg);
    void createLoadInstruction(BytecodeInstructionOpcode opcode, u64 bc_mem_offset, u64 size_in_bytes, s16 reg, RegisterType regtype);
    void createAddressInstruction(VariableDeclarationAST *decl, s16 reg);
    void createLoadOffsetInstruction(ExpressionAST *expr, s16 reg);
    void issue_instruction(BCI *bci);
    void issueReserveStackSpace(u64 size);

    void setPool(PoolAllocator *p) { pool = p; }
    void setInterpreter(Interpreter *i) { interp = i; }
    bytecode_program *compileToBytecode(FileAST *root);
    void compileAllFunctions(FileAST *root);

    void initializeGlobalVariables(Scope *scope);
    void initializeGlobalFunctions(Scope *scope);
    void initializeVariablesInScope(Scope *scope);
    void initializeVariable(VariableDeclarationAST *decl);
    void generate_function(TextType name, FunctionDefinitionAST *fundef);
    void generate_run_directive(RunDirectiveAST *run);
    void generate_statement(StatementAST *stmt);
    void generate_statement_block(StatementBlockAST *block);

    void computeAddressIntoRegister(ExpressionAST *expr, s16 reg);
    void computeExpressionIntoRegister(ExpressionAST *expr, s16 reg);
    void compute_function_call_into_register(FunctionCallAST *funcall, s16 reg);
};

struct bc_call_register
{
    bc_register *regs = nullptr;
    u64 num_regs;
};

struct bytecode_runner
{
    bytecode_program *program = nullptr;
    bc_call_register *current_call_register = nullptr;
    bc_call_register *new_call_register = nullptr;

    void *CallVM = nullptr;

    void run_bc_function(bytecode_function *func);

    void run_preamble();
    void run_directive(RunDirectiveAST *run, PoolAllocator *ast_pool);
    void callExternalFunction(FunctionDefinitionAST *fundef, BCI *bci);
};

void print_bc_program(bytecode_program *program);

// Unify all createStore, we can go the way of pointer, reg style

/*

// This instruction will tell the runner what registers to use when calling the function
BCI *bci = create_instruction(BC_CREATE_CALL_REGISTER, reg, return_regs, argument_qwords);
issue_instruction(bci);

// And now we actually call the function
bci = create_instruction(BC_CALL_PROCEDURE, -1, reg_return, straight_convert(funcall->fundef));
bci->src2_reg = return_regs;
issue_instruction(bci);

+		reg.data._ptr	0x0000017a25d5007d "X is %llu\\n"	unsigned char *

*/