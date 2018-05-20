#pragma once

enum BytecodeInstructionOpcode : u16 {
    BC_UNINITIALIZED,
    BC_NOP, 
    BC_ZERO_REG,
    BC_LOAD_BIG_CONSTANT_TO_REG,  // big const is an arbitrary val
    BC_STORE_TO_STACK_PLUS_CONSTANT,
    BC_STORE_TO_BSS_PLUS_CONSTANT,
    BC_STORE_TO_MEM_PTR,
    BC_LOAD_FROM_STACK_PLUS_CONSTANT,
    BC_LOAD_FROM_BSS_PLUS_CONSTANT,
    BC_LOAD_FROM_CALL_REGISTER,
    BC_LOAD_FROM_MEM_PTR,
    BC_ADDRESS_FROM_STACK_PLUS_CONSTANT,
    BC_ADDRESS_FROM_BSS_PLUS_CONSTANT,
    BC_ADDRESS_FROM_CALL_REGISTER,
    BC_CREATE_CALL_REGISTER,
    BC_CALL_PROCEDURE,
    BC_RETURN,
    BC_BINARY_OPERATION,       // big_const is the op
    BC_UNARY_OPERATION,        // big_const is the op
    BC_RESERVE_STACK_SIZE,
    BC_COPY_REG,
    BC_CAST, 

    BC_STACK_PLUS_CONSTANT, // access stack memory
    BC_BSS_BIG_PLUS_CONSTANT, // access global memory
    BC_DEREFERENCE_REGISTER_PLUS_CONSTANT,
    BC_ASSIGN_TO_BIG_CONSTANT,
    BC_DEREFERENCE_BIG_CONSTANT,
    BC_INTEGER_ADD_TO_CONSTANT,
    BC_INTEGER_ADD_A_TO_B,
    BC_INTEGER_MULTIPLY_A_BY_CONSTANT,
    BC_INTEGER_DIVIDE_A_BY_CONSTANT,
    BC_MAKE_CALLING_RECORD,
    BC_CALLING_RECORD_HAS_BIG_RETURN_VALUE,
    BC_GOTO_CONSTANT_IF_FALSE,
    BC_GOTO_CONSTANT_IF_TRUE,
    BC_GOTO_CONSTANT,
    BC_ARRAY_SUBSCRIPT,
    BC_CAST_NUMBER_TO_NUMBER,
    BC_CAST_INT_TO_POINTER,
    BC_MALLOC,
    BC_FREE,
    BC_STORE_A_THROUGH_REGISTER_AT_SIZE,
    BC_COPY_MEMORY_B_INTO_A_AT_SIZE,
    BC_STORE_A_THROUGH_CONSTANT_AT_SIZE,
    BC_NUMBER_OF_OPERATIONS
};


