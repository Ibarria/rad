#include "bytecode_generator.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <dyncall.h>
#include <dynload.h>

const u64 stack_size = 10 * 1024;

template <class T> const T& max(const T& a, const T& b) {
    return (a<b) ? b : a;     
}

union __xxx_to_u64 {
    f64 f;
    s64 s;
    u64 u;
    void *p;
};

static u64 straight_convert(f64 f) {
    __xxx_to_u64 x;
    x.f = f;
    return x.u;
}

static u64 straight_convert(s64 s) {
    __xxx_to_u64 x;
    x.s = s;
    return x.u;
}

static u64 straight_convert(u64 u) {
    return u;
}

static u64 straight_convert(void *p) {
    __xxx_to_u64 x;
    x.p = p;
    return x.u;
}

static inline u8 truncate_op_size(s64 bytes)
{
    if (bytes < 8) return (u8)bytes;
    return 8;
}

static inline void copy_bytes(u8 *src, u8* dst, u8 count)
{
    if (count == 8) {
        u64 *src8 = (u64 *)src;
        u64 *dst8 = (u64 *)dst;
        *dst8 = *src8;
        return;
    } else if (count == 4) {
        u32 *src4 = (u32 *)src;
        u32 *dst4 = (u32 *)dst;
        *dst4 = *src4;
        return;
    }
    while (count > 0) {
        *dst++ = *src++;
        count--;
    }
}

static inline void copy_bytes(u64 *src, u64 *dst, u8 count)
{
    copy_bytes((u8 *)src, (u8 *)dst, count);
}

static RegisterType get_regtype_from_type(TypeAST *type)
{
    if (type->ast_type == AST_FUNCTION_TYPE) {
        // all functions behave as pointers
        return REGTYPE_POINTER;
    }
    assert(type && type->ast_type == AST_DIRECT_TYPE);
    auto dt = (DirectTypeAST *)type;
    if (dt->isPointer) {
        return REGTYPE_POINTER;
    }
    if (dt->basic_type == BASIC_TYPE_FLOATING) {
        return REGTYPE_FLOAT;
    }
    if (dt->basic_type == BASIC_TYPE_BOOL) {
        return REGTYPE_UINT;
    }
    if (dt->basic_type == BASIC_TYPE_INTEGER) {
        if (dt->isSigned) {
            return REGTYPE_SINT;
        } else {
            return REGTYPE_UINT;
        }
    }
    assert(!"We should never call this with a plain void type or struct or function!");
    return REGTYPE_UNKNOWN;
}

static inline u64 getVariableSize(VariableDeclarationAST *decl)
{
    if ((decl->flags & DECL_FLAG_IS_CONSTANT) &&
        (isFunctionDeclaration(decl) || isStructDeclaration(decl))) {
        // constant functions do not need space
        return 0;
    }
    assert(decl->specified_type->size_in_bytes != 0);
    return decl->specified_type->size_in_bytes;
}

static inline bool isGlobal(VariableDeclarationAST *decl)
{
    assert(decl);
    assert(decl->scope);
    if (decl->scope->parent == nullptr) {
        assert(!(decl->flags & DECL_FLAG_IS_LOCAL_VARIABLE));
        assert(!(decl->flags & DECL_FLAG_IS_FUNCTION_ARGUMENT));
        assert(decl->flags & DECL_FLAG_IS_GLOBAL_VARIABLE);
    } else {
        assert(decl->flags & (DECL_FLAG_IS_LOCAL_VARIABLE | DECL_FLAG_IS_FUNCTION_ARGUMENT));
    }
    return !!(decl->flags & DECL_FLAG_IS_GLOBAL_VARIABLE);
}

static inline bool isLocal(VariableDeclarationAST *decl)
{
    assert(decl);
    return !!(decl->flags & DECL_FLAG_IS_LOCAL_VARIABLE);
}

static inline bool isArgument(VariableDeclarationAST *decl)
{
    assert(decl);
    return !!(decl->flags & DECL_FLAG_IS_FUNCTION_ARGUMENT);
}

static inline s16 roundToQWord(u64 bytes)
{
    assert(bytes > 0);
    u64 qwcount = bytes / 8;
    if (bytes % 8 != 0) qwcount++;
    assert(qwcount == (s16)qwcount);
    return (s16)qwcount;
}

static inline s16 reserveRegistersForSize(bytecode_machine *machine, u64 size_in_bytes)
{
    // @TODO: Possible optimization, for large structs or arrays,
    // store them in the stack and just copy here pointers to that
    return machine->reserve_register(roundToQWord(size_in_bytes));
}

static u64 getScopeVariablesSize(Scope *scope)
{
    u64 total_size = 0;
    
    // this only works for the top scope for now due to computing the total_size
    // might work for stack... who knows
    assert(scope->parent == nullptr);

    for (auto decl : scope->decls) {
        if (decl->flags & DECL_FLAG_IS_TYPE) {
            // Type variables do not take space
            // Possible optimization, same for const variables
            continue;
        }
        assert(decl->specified_type);
        assert(decl->specified_type->size_in_bytes > 0);
        
        u64 var_size = getVariableSize(decl);

        if (var_size > 0) {
            // the offset for global variables is the accumulated size in bytes
            decl->bc_mem_offset = total_size;
            
            total_size += decl->specified_type->size_in_bytes;
        }
    }
    return total_size;
}

static inline u64 roundToPage(u64 size, u64 page_size)
{
    size = (page_size - 1)&size ? ((size + page_size) & ~(page_size - 1)) : size;
    return size;
}

static inline u64 computeOffset(VarReferenceAST *var_ref)
{
    u64 off;

    // top start offset of the struct or group of structs
    off = var_ref->decl->bc_mem_offset; 

    auto vnext = var_ref->next;
    do {
        off += vnext->decl->bc_mem_offset;
        vnext = vnext->next;
    } while (vnext != nullptr);
    return off;
}

static BytecodeInstructionOpcode getStoreOpcode(VariableDeclarationAST *decl)
{
    BytecodeInstructionOpcode opcode;
    if (isGlobal(decl)) {
        opcode = BC_STORE_TO_BSS_PLUS_CONSTANT;
    } else if (isLocal(decl)) {
        opcode = BC_STORE_TO_STACK_PLUS_CONSTANT;
    } else {
        assert(!"Variable without scope set!");
    }
    return opcode;
}

static BytecodeInstructionOpcode getLoadOpcode(VariableDeclarationAST *decl)
{
    BytecodeInstructionOpcode opcode;
    if (isGlobal(decl)) {
        opcode = BC_LOAD_FROM_BSS_PLUS_CONSTANT;
    } else if (isLocal(decl)) {
        opcode = BC_LOAD_FROM_STACK_PLUS_CONSTANT;
    } else if (isArgument(decl)) {
        opcode = BC_LOAD_FROM_CALL_REGISTER;
    } else {
        assert(!"Variable without scope set!");
    }
    return opcode;
}

#define CASE_BC_OPCODE(a) case a: return #a
const char *bc_opcode_to_str(BytecodeInstructionOpcode opcode)
{
    switch(opcode)
    {
        CASE_BC_OPCODE(BC_UNINITIALIZED);
        CASE_BC_OPCODE(BC_ZERO_REG);
        CASE_BC_OPCODE(BC_LOAD_BIG_CONSTANT_TO_REG);
        CASE_BC_OPCODE(BC_STORE_TO_STACK_PLUS_CONSTANT);
        CASE_BC_OPCODE(BC_STORE_TO_BSS_PLUS_CONSTANT);
        CASE_BC_OPCODE(BC_LOAD_FROM_STACK_PLUS_CONSTANT);
        CASE_BC_OPCODE(BC_LOAD_FROM_BSS_PLUS_CONSTANT);
        CASE_BC_OPCODE(BC_LOAD_FROM_CALL_REGISTER);
        CASE_BC_OPCODE(BC_CREATE_CALL_REGISTER);
        CASE_BC_OPCODE(BC_CALL_PROCEDURE);
        CASE_BC_OPCODE(BC_RETURN);
        CASE_BC_OPCODE(BC_BINARY_OPERATION);
        CASE_BC_OPCODE(BC_UNARY_OPERATION);
        CASE_BC_OPCODE(BC_RESERVE_STACK_SIZE);
        default:
            assert(!"Unknown bytecode Instruction opcode, please add");
            return "UNKNOWN OPCODE";
    }
}
static const char *RegTypeToStr(RegisterType rt)
{
    switch (rt)
    {
        CASE_BC_OPCODE(REGTYPE_UNKNOWN);
        CASE_BC_OPCODE(REGTYPE_UINT);
        CASE_BC_OPCODE(REGTYPE_SINT);
        CASE_BC_OPCODE(REGTYPE_FLOAT);
        CASE_BC_OPCODE(REGTYPE_POINTER);
    default:
        assert(false);
        return "XXX%%";
    }
}

void print_instruction(BCI *inst)
{
    printf("  op: %s src: %d src2: %d dst: %d type: %s size: %d big_const: %" U64FMT "u | 0x%" U64FMT "X\n",
           bc_opcode_to_str(inst->opcode), (int)inst->src_reg, (int)inst->src2_reg,
           (int)inst->dst_reg, RegTypeToStr(inst->dst_type), inst->dst_type_bytes, 
           inst->big_const, inst->big_const);
}

void print_bc_function(bytecode_function *func)
{
    if (func->function_name) {
        printf("Function : %s ", func->function_name);
    } else {
        printf("Function <unnamed> ");
    }
    printf(" local var size: %" U64FMT "u num_instructions: %d\n",
           func->bc_params_size, func->instructions.size());
    for(auto inst: func->instructions) {
        print_instruction(inst);
    }
}

void print_bc_program(bytecode_program *program)
{
    printf("Preamble function:\n");
    print_bc_function(&program->preamble_function);

    printf("Start function: ");
    if (program->start_function) {
        printf("%s\n", program->start_function->function_name);
    } else {
        printf(" <none>\n");
    }
    
    for (auto func:program->functions) {
        print_bc_function(func);
    }
}

void bytecode_generator::createStoreInstruction(BytecodeInstructionOpcode opcode, u64 bc_mem_offset, 
                                                u64 size_in_bytes, s16 reg, RegisterType regtype)
{
    BCI *bci;
    
    bci = create_instruction(opcode, reg, -1, bc_mem_offset);
    bci->dst_type_bytes = truncate_op_size(size_in_bytes);
    bci->dst_type = regtype;
    issue_instruction(bci);
    if (size_in_bytes > 8) {
        s64 bytes = size_in_bytes;
        u64 offset = bc_mem_offset + 8;
        bytes -= 8;
        reg++;
        do {
            BCI *bci = create_instruction(opcode, reg, -1, offset);
            issue_instruction(bci);
            bci->dst_type_bytes = truncate_op_size(bytes);
            bci->dst_type = regtype;
            reg++;
            bytes -= 8;
            offset += 8;
        } while (bytes > 0);
    }
}

void bytecode_generator::createStoreInstruction(VariableDeclarationAST *decl, s16 reg)
{
    BytecodeInstructionOpcode opcode = getStoreOpcode(decl);
    
    RegisterType regtype;
    regtype = get_regtype_from_type(decl->specified_type);

    createStoreInstruction(opcode, decl->bc_mem_offset,
                           decl->specified_type->size_in_bytes, reg, regtype);
}

void bytecode_generator::createLoadInstruction(BytecodeInstructionOpcode opcode, u64 bc_mem_offset, 
                                               u64 size_in_bytes, s16 reg, RegisterType regtype)
{
    BCI *bci;

    bci = create_instruction(opcode, -1, reg, bc_mem_offset);
    bci->dst_type_bytes = truncate_op_size(size_in_bytes);
    bci->dst_type = regtype;
    issue_instruction(bci);
    if (size_in_bytes > 8) {
        s64 bytes = size_in_bytes;
        u64 offset = bc_mem_offset + 8;
        bytes -= 8;
        reg++;
        do {
            BCI *bci = create_instruction(opcode, -1, reg, offset);
            issue_instruction(bci);
            bci->dst_type_bytes = truncate_op_size(bytes);
            bci->dst_type = regtype;
            reg++;
            bytes -= 8;
            offset += 8;
        } while (bytes > 0);
    }
}

void bytecode_generator::createLoadInstruction(VariableDeclarationAST *decl, s16 reg)
{
    BytecodeInstructionOpcode opcode = getLoadOpcode(decl);

    RegisterType regtype = get_regtype_from_type(decl->specified_type);

    createLoadInstruction(opcode, decl->bc_mem_offset,
        decl->specified_type->size_in_bytes, reg, regtype);
}


external_library * bytecode_generator::findOrLoadLibrary(TextType filename)
{
    char lib_name[128] = {};
    // very hacky: to make the library name, replace 'jai' for 'dll' at the end of the string
    u64 l = strlen(filename);
    strcpy(lib_name, filename);
    assert(l >= 5);
    assert(lib_name[l - 3] == 'j');  assert(lib_name[l - 2] == 'a'); assert(lib_name[l - 1] == 'i');
    lib_name[l - 3] = 'd'; lib_name[l - 2] = 'l'; lib_name[l - 1] = 'l';

    for (auto lib : program->external_libs) {
        if (!strcmp(lib->name, lib_name)) {
            return lib;
        }
    }
    // If we are here, we need to load the library
    external_library *lib = new (pool) external_library;
    lib->name = CreateTextType(pool, lib_name);
    lib->dll = dlLoadLibrary(lib_name);
    assert(lib->dll != nullptr);
    program->external_libs.push_back(lib);
    return lib;
}

BCI * bytecode_generator::create_instruction(BytecodeInstructionOpcode opcode, s16 src_reg, s16 dst_reg, u64 big_const)
{
    BCI *bci = new (pool) BCI;
    bci->opcode = opcode;
    bci->src_reg = src_reg;
    bci->dst_reg = dst_reg;
    bci->big_const = big_const;

    return bci;
}

void bytecode_generator::issue_instruction(BCI * bci)
{
    current_function->instructions.push_back(bci);
}

void bytecode_generator::issueReserveStackSpace(u64 size)
{
    BCI *bci = create_instruction(BC_RESERVE_STACK_SIZE, 0, 0, size);
    bci->dst_type_bytes = 8;
    issue_instruction(bci);
}

bytecode_program * bytecode_generator::compileToBytecode(FileAST * root)
{
    bytecode_program *bp = new (pool) bytecode_program;
    this->program = bp;

    // First step, have space in the bss for the global variables (non functions)
    u64 bss_size = getScopeVariablesSize(&root->global_scope);
    // ensure we get page aligned memory chunks
    bss_size = roundToPage(bss_size, 4 * 1024);

    bp->bss.initMem((u8 *)pool->alloc(bss_size + stack_size),
        bss_size, stack_size);
    
    // set the correct value in the bss area for vars (bytecode instruction)
    current_function = &program->preamble_function;
    initializeVariablesInScope(&root->global_scope);
    current_function = nullptr;
    
    // Update function variable addresses?
    
    return bp;
}

void bytecode_generator::generate_function(TextType name, FunctionDefinitionAST * fundef)
{
    if (fundef->declaration->isForeign) {
        assert(fundef->declaration->func_ptr == nullptr);

        external_library *exlib = findOrLoadLibrary(fundef->declaration->filename);
        fundef->declaration->func_ptr = dlFindSymbol((DLLib *)exlib->dll, fundef->var_decl->varname);
        assert(fundef->declaration->func_ptr);
        // During bytecode processing is the best time to go and ensure the function is setup
        return; // foreign functions just get called
    }

    bytecode_function *old_current = current_function;
    bytecode_function *func = new (pool) bytecode_function;
    func->function_name = name;
    func->function_id = fundef->s;
    current_function = func;
	
    assert(fundef->declaration);
    auto fundecl = fundef->declaration;
    if (!isVoidType(fundecl->return_type)) {
        // Return types go first in the calling record
        u32 var_size = fundecl->return_type->size_in_bytes;
        current_function->bc_params_size += var_size;
    }
    for (auto arg: fundecl->arguments) {
        // compute the size and offset for the arguments
        assert(arg->specified_type);
        u64 var_size = getVariableSize(arg);
        arg->bc_mem_offset = current_function->bc_params_size;
        current_function->bc_params_size += var_size;
    }
    
    // Creating a function is the same as processing its statementBlock
    generate_statement_block(fundef->function_body);
	
	current_function = old_current;
    if (!strcmp(name, "main") && (fundef->scope->parent == nullptr)) {
        program->start_function = func;
    }
    assert(fundef->bc_function == nullptr);
    fundef->bc_function = func;
    program->functions.push_back(func);
}

void bytecode_generator::generate_statement_block(StatementBlockAST *block)
{
    // first thing, allocate space for the variables, and process implicit
    // initialization
    for (auto stmt: block->statements) {
        switch (stmt->ast_type) {
            case AST_VARIABLE_DECLARATION: {
                auto decl = (VariableDeclarationAST *)stmt;
                u64 var_size = getVariableSize(decl);
                if (var_size > 0) {
                    // Possible optimization: have stack space in a per block basis
                    // We might need this for defers too. 
                    // reserve space and figure out where it is
                    decl->bc_mem_offset = current_function->bc_params_size;
                    current_function->bc_params_size += var_size;
                    issueReserveStackSpace(var_size);
                }
                initializeVariable(decl);
                break;
            }
            case AST_ASSIGNMENT: {
                auto assign = (AssignmentAST *)stmt;
                s16 mark = program->machine.reg_mark();
                s16 reg = reserveRegistersForSize(&program->machine, assign->lhs->expr_type->size_in_bytes);
                computeExpressionIntoRegister(assign->rhs, reg);

                if (assign->lhs->ast_type == AST_IDENTIFIER) {
                    auto iden = (IdentifierAST *)assign->lhs;
                    createStoreInstruction(iden->decl, reg);
                } else if (assign->lhs->ast_type == AST_VAR_REFERENCE) {
                    auto var_ref = (VarReferenceAST *)assign->lhs;
                    u64 var_offset = computeOffset(var_ref);
                    assert(var_ref->size_in_bytes > 0);
                    createStoreInstruction(getStoreOpcode(var_ref->decl), var_offset,
                        var_ref->size_in_bytes, reg, get_regtype_from_type(var_ref->expr_type));
                } else {
                    assert (! "We do not support anything else than an identifier for lhs for now");
                }
                
                program->machine.pop_mark(mark);
                break;
            }
            case AST_FUNCTION_CALL: {
                auto funcall = (FunctionCallAST *)stmt;
                // on statements we do not care about return values
                compute_function_call_into_register(funcall, -1);
                break;
            }
            case AST_RETURN_STATEMENT: {
                auto ret_stmt = (ReturnStatementAST *)stmt;
                s16 mark = program->machine.reg_mark();
                s16 reg = reserveRegistersForSize(&program->machine, ret_stmt->ret->expr_type->size_in_bytes);
                computeExpressionIntoRegister(ret_stmt->ret, reg);
                createStoreInstruction(BC_STORE_TO_STACK_PLUS_CONSTANT, 0, ret_stmt->ret->expr_type->size_in_bytes, 
                    reg, get_regtype_from_type(ret_stmt->ret->expr_type));
                BCI *bci = create_instruction(BC_RETURN, -1, -1, 0);
                issue_instruction(bci);
                program->machine.pop_mark(mark);
                break;
            }
            case AST_STATEMENT_BLOCK: {
                auto inner_block = (StatementBlockAST *)stmt;
                generate_statement_block(inner_block);
                break;
            }
            default:
                assert(!"Generate Statement Block in Bytecode, unknown AST");
        }
    }
}

void bytecode_generator::initializeVariablesInScope(Scope * scope)
{
    for (auto decl : scope->decls) {
        initializeVariable(decl);
    }
}

void bytecode_generator::initializeVariable(VariableDeclarationAST * decl)
{
    if (!decl->definition) return; // nothing to do for this var
    // @TODO: initialize it to 0 by default
    
    if (decl->definition->ast_type == AST_FUNCTION_DEFINITION) {
        auto fundef = (FunctionDefinitionAST *)decl->definition;
        generate_function(decl->varname, fundef);
        return;
    }
    if (decl->definition->ast_type == AST_STRUCT_DEFINITION) {
        // struct definitions do not cause any code to be created
        // when they are instantiated, yes
        return;
    }
    // @TODO: we do not support function pointers yet

    // ok, it is an expression (operation, literal (num, string) )
    // or god forbid, another variable (which means load its value
    s16 mark = program->machine.reg_mark();
    s16 reg = reserveRegistersForSize(&program->machine, decl->specified_type->size_in_bytes);
    
    computeExpressionIntoRegister((ExpressionAST *)decl->definition, reg);
    createStoreInstruction(decl, reg);
    
    program->machine.pop_mark(mark);
}

void bytecode_generator::computeExpressionIntoRegister(ExpressionAST * expr, s16 reg)
{
    switch (expr->ast_type) {
    case AST_LITERAL: {
        auto lit = (LiteralAST *)expr;

        switch (lit->typeAST.basic_type) {
        case BASIC_TYPE_INTEGER: {
            u64 val;
            if (lit->typeAST.isSigned) val = straight_convert(lit->_s64);
            else val = lit->_u64;
            BCI *bci = create_instruction(BC_LOAD_BIG_CONSTANT_TO_REG, -1, reg, val);
            bci->dst_type_bytes = lit->typeAST.size_in_bytes;
            bci->dst_type = (lit->typeAST.isSigned ? REGTYPE_SINT : REGTYPE_UINT);
            issue_instruction(bci);
            break;
        }
        case BASIC_TYPE_STRING: {
            // Hacky version, the string data will use the pointer in the AST... 
            // @TODO: think of a better version to handle strings, such as:
            /*
               - Strings in some data segment (all the ones in the program?)
               - allocate memory for all the strings, malloc style
               - Inline the strings (if known size), right after the pointer or such
               - Are strings mutable? How does allocation work? Do we care?
            */
            u64 val = straight_convert(lit->str);
            BCI *bci = create_instruction(BC_LOAD_BIG_CONSTANT_TO_REG, -1, reg, val);
            bci->dst_type_bytes = 8;
            bci->dst_type = REGTYPE_POINTER;
            issue_instruction(bci);
            val = strlen(lit->str);
            bci = create_instruction(BC_LOAD_BIG_CONSTANT_TO_REG, -1, reg+1, val);
            bci->dst_type_bytes = 8;
            bci->dst_type = REGTYPE_UINT;
            issue_instruction(bci);
            break;
        }
        case BASIC_TYPE_BOOL: {
            BCI *bci = create_instruction(BC_LOAD_BIG_CONSTANT_TO_REG, -1, reg, lit->_bool);
            bci->dst_type_bytes = lit->typeAST.size_in_bytes;
            bci->dst_type = REGTYPE_UINT;
            issue_instruction(bci);
            break;
        }
        case BASIC_TYPE_FLOATING: {
            u64 val = straight_convert(lit->_f64);
            BCI *bci = create_instruction(BC_LOAD_BIG_CONSTANT_TO_REG, -1, reg, val);
            bci->dst_type_bytes = lit->typeAST.size_in_bytes;
            bci->dst_type = REGTYPE_FLOAT;
            issue_instruction(bci);
            break;
        }
        default:
            assert(false);
        }
        break;
    }
    case AST_IDENTIFIER: {
        auto id = (IdentifierAST *)expr;
        assert(id->decl);
        createLoadInstruction(id->decl, reg);
        
        break;
    }
    case AST_UNARY_OPERATION: {
        auto unop = (UnaryOperationAST *)expr;
        assert(!"Unary expression not supported on bytecode yet");
        break;
    }
    case AST_BINARY_OPERATION: {
        auto binop = (BinaryOperationAST *)expr;
        // Basic idea: 
        // Do recursive bytecode for op1, op2 in some alloc registers
        // then convert the opcode into some bytecode call (maybe just 1 inst)
        // operate on the 2 regs, write into the result
        s16 mark = program->machine.reg_mark();
        TypeAST *lhsType = binop->lhs->expr_type;
        TypeAST *rhsType = binop->rhs->expr_type;
        s16 reglhs = reserveRegistersForSize(&program->machine, lhsType->size_in_bytes);
        s16 regrhs = reserveRegistersForSize(&program->machine, rhsType->size_in_bytes);        
        computeExpressionIntoRegister(binop->lhs, reglhs);
        // @Optimization: do shortcircuit?
        computeExpressionIntoRegister(binop->rhs, regrhs);
        BCI *bci = create_instruction(BC_BINARY_OPERATION, reglhs, reg, binop->op);
        bci->src2_reg = regrhs;
        bci->dst_type_bytes = expr->expr_type->size_in_bytes;
        bci->dst_type = get_regtype_from_type(expr->expr_type);

        issue_instruction(bci);
        program->machine.pop_mark(mark);
        break;
    }
    case AST_ASSIGNMENT: {
        assert(!"Assignment expression bytecode not implemented");
        break;
    }
    case AST_FUNCTION_CALL: {
        auto funcall = (FunctionCallAST *)expr;
        assert(!isVoidType(funcall->fundef->declaration->return_type));
        compute_function_call_into_register(funcall, reg);
        break;
    }
    default:
        assert(!"Unknown expression AST for bytecode");
    }
}

void bytecode_generator::compute_function_call_into_register(FunctionCallAST *funcall, s16 reg_return)
{
    auto fundecl = funcall->fundef->declaration;
    u64 argument_qwords = 0;

    // @TODO: expand this when we support implicit arguments
    
    // For now, we only support calling a function with as many arguments as declared
    // Relax this part... 
    // assert(funcall->args.size() == fundecl->arguments.size());

    if (!isVoidType(fundecl->return_type)) {
        argument_qwords += roundToQWord(fundecl->return_type->size_in_bytes);
    }

    s16 mark = program->machine.reg_mark();
    for (u32 index = 0; index < funcall->args.size(); index++) {
        auto arg_expr = funcall->args[index];
        // Here we assume that the type is the same on arg_expr and arg_decl
        argument_qwords += arg_expr->expr_type->size_in_bytes;
    }

    // Now we reserve enough registers for all the args
    s16 reg = reserveRegistersForSize(&program->machine, argument_qwords);
    s16 offset = 0;
    if (!isVoidType(fundecl->return_type)) {
        offset += roundToQWord(fundecl->return_type->size_in_bytes);
        for (u32 ind = 0; ind < offset; ind++) {
            BCI *bci = create_instruction(BC_ZERO_REG, -1, reg+ind, 0);
            bci->dst_type = REGTYPE_UINT;
            bci->dst_type_bytes = 8;
        }
    }

    for (u32 index = 0; index < funcall->args.size(); index++) {
        auto arg_expr = funcall->args[index];

        computeExpressionIntoRegister(arg_expr, reg + offset);
        //createStoreInstruction(BC_STORE_TO_CALL_REGISTER, offset,
        //    arg_decl->specified_type->size_in_bytes, reg);

        // advance the offset for the next expression to write
        offset += roundToQWord(arg_expr->expr_type->size_in_bytes);
    }

    // We compute and pass the total bytes for the arguments in order to handle 
    // foreign functions like printf, where we need to know how to load the stack
    // At this point, the registers from reg until the argument_bytes have 

    // This instruction will tell the runner what registers to use when calling the function
    BCI *bci = create_instruction(BC_CREATE_CALL_REGISTER, reg, -1, argument_qwords);
    issue_instruction(bci);

    // And now we actually call the function
    bci = create_instruction(BC_CALL_PROCEDURE, -1, reg_return, straight_convert(funcall->fundef));
    issue_instruction(bci);
    bci->dst_type_bytes = fundecl->return_type->size_in_bytes;
    program->machine.pop_mark(mark);
}


void bc_base_memory::initMem(u8 * basemem, u64 bss_size, u64 stack_size)
{
    mem = basemem;
    alloc_size = bss_size + stack_size;
    used_size = 0;
    stack_start = mem + bss_size;
    stack_base = stack_pointer = stack_start;
    this->stack_size = stack_size;
}

void bytecode_runner::run_bc_function(bytecode_function * func)
{
    for (auto bci : func->instructions) {
        switch (bci->opcode) {
        case BC_ZERO_REG: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            program->machine.regs[bci->dst_reg].data._u64 = 0;
            program->machine.regs[bci->dst_reg].bytes = bci->dst_type_bytes;
            program->machine.regs[bci->dst_reg].type = bci->dst_type;
            break;
        }
        case BC_LOAD_BIG_CONSTANT_TO_REG: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            copy_bytes(&bci->big_const, &program->machine.regs[bci->dst_reg].data._u64, bci->dst_type_bytes);
            program->machine.regs[bci->dst_reg].bytes = bci->dst_type_bytes;
            program->machine.regs[bci->dst_reg].type = bci->dst_type;
            break;
        }
        case BC_STORE_TO_STACK_PLUS_CONSTANT: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert(program->bss.stack_start + program->bss.stack_size > program->bss.stack_base + bci->big_const);
            copy_bytes((u8*)(&program->machine.regs[bci->src_reg].data._u64),
                &program->bss.stack_base[bci->big_const],
                bci->dst_type_bytes);
            break;
        }
        //case BC_STORE_TO_TOP_STACK_PLUS_CONSTANT: {
        //    assert(program->bss.stack_start + program->bss.stack_size > program->bss.stack_pointer + bci->big_const);
        //    copy_bytes((u8*)(&program->machine.regs[bci->src_reg].data._u64),
        //        &program->bss.stack_pointer[bci->big_const],
        //        bci->dst_type_bytes);
        //    break;
        //}
        case BC_STORE_TO_BSS_PLUS_CONSTANT: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert(bci->big_const < program->bss.alloc_size);
            copy_bytes((u8 *)&program->machine.regs[bci->src_reg].data._u64, 
                &program->bss.mem[bci->big_const],
                bci->dst_type_bytes);
            break;
        }
        case BC_LOAD_FROM_STACK_PLUS_CONSTANT: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert(program->bss.stack_start + program->bss.stack_size > program->bss.stack_base + bci->big_const);
            copy_bytes(&program->bss.stack_base[bci->big_const],
                (u8 *)&program->machine.regs[bci->dst_reg].data._u64,
                bci->dst_type_bytes);
            program->machine.regs[bci->dst_reg].bytes = bci->dst_type_bytes;
            program->machine.regs[bci->dst_reg].type = bci->dst_type;
            break;
        }
        case BC_LOAD_FROM_BSS_PLUS_CONSTANT: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert(bci->big_const < program->bss.alloc_size);
            copy_bytes(&program->bss.mem[bci->big_const], 
                (u8 *)&program->machine.regs[bci->dst_reg].data._u64, 
                bci->dst_type_bytes);
            program->machine.regs[bci->dst_reg].bytes = bci->dst_type_bytes;
            program->machine.regs[bci->dst_reg].type = bci->dst_type;
            break;
        }
        case BC_LOAD_FROM_CALL_REGISTER: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert(current_call_register);
            assert(current_call_register->num_regs > bci->big_const);
            copy_bytes(&current_call_register->regs[bci->big_const].data._u64,
                &program->machine.regs[bci->dst_reg].data._u64,
                bci->dst_type_bytes);
            program->machine.regs[bci->dst_reg].bytes = bci->dst_type_bytes;
            program->machine.regs[bci->dst_reg].type = bci->dst_type;
            break;
        }
        case BC_RESERVE_STACK_SIZE: {
            program->bss.stack_pointer += bci->big_const;
            assert(program->bss.stack_pointer < program->bss.stack_start + program->bss.stack_size);
            break;
        }
        case BC_BINARY_OPERATION: {
            auto &dstreg = program->machine.regs[bci->dst_reg];
            auto &srcreg = program->machine.regs[bci->src_reg];
            auto &src2reg = program->machine.regs[bci->src2_reg];

            assert(bci->dst_type != REGTYPE_UNKNOWN);

            // at this level, types have to match. It is up to the higher levels to put casts in
            // bytecode
            assert(srcreg.type == src2reg.type);
            assert(srcreg.bytes == src2reg.bytes);

#define BOOL_OP_REG(dst, src, src2, op)                     \
    if (src.type == REGTYPE_UINT) {                         \
        if (src.bytes == 1) {                               \
            dst.data._u8 = src.data._u8 op src2.data._u8;   \
        } else if (src.bytes == 2) {                        \
            dst.data._u8 = src.data._u16 op src2.data._u16; \
        } else if (src.bytes == 4) {                        \
            dst.data._u8 = src.data._u32 op src2.data._u32; \
        } else {                                            \
            assert(src.bytes == 8);                         \
            dst.data._u8 = src.data._u64 op src2.data._u64; \
        }                                                   \
    } else if (src.type == REGTYPE_SINT) {                  \
        if (src.bytes == 1) {                               \
            dst.data._u8 = src.data._s8 op src2.data._s8;   \
        } else if (src.bytes == 2) {                        \
            dst.data._u8 = src.data._s16 op src2.data._s16; \
        } else if (src.bytes == 4) {                        \
            dst.data._u8 = src.data._s32 op src2.data._s32; \
        } else {                                            \
            assert(src.bytes == 8);                         \
            dst.data._u8 = src.data._s64 op src2.data._s64; \
        }                                                   \
    } else if (src.type == REGTYPE_FLOAT) {                 \
        if (src.bytes == 4) {                               \
            dst.data._u8 = src.data._f32 op src2.data._f32; \
        } else {                                            \
            assert(src.bytes == 8);                         \
            dst.data._u8 = src.data._f64 op src2.data._f64; \
        }                                                   \
    } else {                                                \
        assert(src.type == REGTYPE_POINTER);                \
        dst.data._u8 = src.data._ptr op src2.data._ptr;     \
    }

#define BINOP_UINT(dst, src, src2, op)                     \
    if (src.bytes == 1) {                                  \
        dst.data._u8 = src.data._u8 op src2.data._u8;      \
    } else if (src.bytes == 2) {                           \
        dst.data._u16 = src.data._u16 op src2.data._u16;   \
    } else if (src.bytes == 4) {                           \
        dst.data._u32 = src.data._u32 op src2.data._u32;   \
    } else {                                               \
        assert(src.bytes == 8);                            \
        dst.data._u64 = src.data._u64 op src2.data._u64;   \
    }                                                      

#define BINOP_SINT(dst, src, src2, op)                     \
    if (src.bytes == 1) {                                  \
        dst.data._s8 = src.data._s8 op src2.data._s8;      \
    } else if (src.bytes == 2) {                           \
        dst.data._s16 = src.data._s16 op src2.data._s16;   \
    } else if (src.bytes == 4) {                           \
        dst.data._s32 = src.data._s32 op src2.data._s32;   \
    } else {                                               \
        assert(src.bytes == 8);                            \
        dst.data._s64 = src.data._s64 op src2.data._s64;   \
    }                                                      

#define BINOP_FLOAT(dst, src, src2, op)                    \
    if (src.bytes == 4) {                                  \
        dst.data._f32 = src.data._f32 op src2.data._f32;   \
    } else {                                               \
        assert(src.bytes == 8);                            \
        dst.data._f64 = src.data._f64 op src2.data._f64;   \
    }                                                      

            switch (bci->big_const) {
            case TK_EQ: {
                BOOL_OP_REG(dstreg, srcreg, src2reg, ==);
                // assign bool type to the register
                dstreg.type = REGTYPE_UINT;
                dstreg.bytes = 1;
                break;
            }
            case TK_LEQ: {
                BOOL_OP_REG(dstreg, srcreg, src2reg, <= );
                dstreg.type = REGTYPE_UINT;
                dstreg.bytes = 1;
                break;
            }
            case TK_GEQ: {
                BOOL_OP_REG(dstreg, srcreg, src2reg, >= );
                dstreg.type = REGTYPE_UINT;
                dstreg.bytes = 1;
                break;
            }
            case TK_NEQ: {
                BOOL_OP_REG(dstreg, srcreg, src2reg, != );
                dstreg.type = REGTYPE_UINT;
                dstreg.bytes = 1;
                break;
            }
            case TK_LT: {
                BOOL_OP_REG(dstreg, srcreg, src2reg, < );
                dstreg.type = REGTYPE_UINT;
                dstreg.bytes = 1;
                break;
            }
            case TK_GT: {
                BOOL_OP_REG(dstreg, srcreg, src2reg, > );
                dstreg.type = REGTYPE_UINT;
                dstreg.bytes = 1;
                break;
            }
            case TK_RSHIFT: {
                assert((srcreg.type == REGTYPE_UINT) || (srcreg.type == REGTYPE_SINT));
                if (srcreg.type == REGTYPE_UINT) {
                    BINOP_UINT(dstreg, srcreg, src2reg, >> );
                } else if (srcreg.type == REGTYPE_SINT) {
                    BINOP_SINT(dstreg, srcreg, src2reg, >> );
                }

                dstreg.type = srcreg.type;
                dstreg.bytes = srcreg.bytes;

                break;
            }
            case TK_LSHIFT: {
                assert((srcreg.type == REGTYPE_UINT) || (srcreg.type == REGTYPE_SINT));
                if (srcreg.type == REGTYPE_UINT) {
                    BINOP_UINT(dstreg, srcreg, src2reg, << );
                } else if (srcreg.type == REGTYPE_SINT) {
                    BINOP_SINT(dstreg, srcreg, src2reg, << );
                }

                dstreg.type = srcreg.type;
                dstreg.bytes = srcreg.bytes;

                break;
            }
            case TK_STAR: {
                assert(srcreg.type != REGTYPE_POINTER);
                if (srcreg.type == REGTYPE_UINT) {
                    BINOP_UINT(dstreg, srcreg, src2reg, * );
                } else if (srcreg.type == REGTYPE_SINT) {
                    BINOP_SINT(dstreg, srcreg, src2reg, * );
                } else if (srcreg.type == REGTYPE_FLOAT) {
                    BINOP_FLOAT(dstreg, srcreg, src2reg, *);
                }

                dstreg.type = srcreg.type;
                dstreg.bytes = srcreg.bytes;

                break;
            }
            case TK_DIV: {
                assert(srcreg.type != REGTYPE_POINTER);
                if (srcreg.type == REGTYPE_UINT) {
                    BINOP_UINT(dstreg, srcreg, src2reg, /);
                } else if (srcreg.type == REGTYPE_SINT) {
                    BINOP_SINT(dstreg, srcreg, src2reg, /);
                } else if (srcreg.type == REGTYPE_FLOAT) {
                    BINOP_FLOAT(dstreg, srcreg, src2reg, /);
                }

                dstreg.type = srcreg.type;
                dstreg.bytes = srcreg.bytes;

                break;
            }
            case TK_MOD: {
                assert((srcreg.type == REGTYPE_UINT) || (srcreg.type == REGTYPE_SINT));
                if (srcreg.type == REGTYPE_UINT) {
                    BINOP_UINT(dstreg, srcreg, src2reg, % );
                } else if (srcreg.type == REGTYPE_SINT) {
                    BINOP_SINT(dstreg, srcreg, src2reg, % );
                }

                dstreg.type = srcreg.type;
                dstreg.bytes = srcreg.bytes;

                break;
            }
            case TK_PLUS: {
                if (srcreg.type == REGTYPE_UINT) {
                    BINOP_UINT(dstreg, srcreg, src2reg, +);
                } else if (srcreg.type == REGTYPE_SINT) {
                    BINOP_SINT(dstreg, srcreg, src2reg, +);
                } else if (srcreg.type == REGTYPE_FLOAT) {
                    BINOP_FLOAT(dstreg, srcreg, src2reg, +);
                } else if (srcreg.type == REGTYPE_POINTER) {
                    assert(!"Not implemented yet");
                    /*
                    This should not be too bad, just ensure the 2nd operand is an integer, 
                    any size, and then do the operation, freezing the first op
                    Well, the type of the pointer matters, but we could push to the upper
                    layer do multiply the second operand by the size of the ptr type first
                    */
                }

                dstreg.type = srcreg.type;
                dstreg.bytes = srcreg.bytes;

                break;
            }
            case TK_MINUS: {
                if (srcreg.type == REGTYPE_UINT) {
                    BINOP_UINT(dstreg, srcreg, src2reg, +);
                } else if (srcreg.type == REGTYPE_SINT) {
                    BINOP_SINT(dstreg, srcreg, src2reg, +);
                } else if (srcreg.type == REGTYPE_FLOAT) {
                    BINOP_FLOAT(dstreg, srcreg, src2reg, +);
                } else if (srcreg.type == REGTYPE_POINTER) {
                    assert(!"Not implemented yet");
                    /*
                    This should not be too bad, just ensure the 2nd operand is an integer,
                    any size, and then do the operation, freezing the first op
                    Well, the type of the pointer matters, but we could push to the upper
                    layer do multiply the second operand by the size of the ptr type first
                    */
                }

                dstreg.type = srcreg.type;
                dstreg.bytes = srcreg.bytes;

                break;
            }
            default:
                assert(!"Unknown operator for a binary operation");
                break;
            }
            break;
        }
        case BC_UNARY_OPERATION: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert(!"Instruction not implemented");
            break;
        }
        case BC_CREATE_CALL_REGISTER: {
            // Remeber to issue a zero reg for the first reg if there is a return value!
            // (or a number of them)
            assert(bci->big_const < 256); // Sane value... 
            bc_call_register *call = new bc_call_register;
            call->regs = new bc_register[bci->big_const];
            call->num_regs = bci->big_const;
            assert(new_call_register == nullptr);
            for (s16 ind = 0; ind < bci->big_const; ind++) {
                s16 srcreg = bci->src_reg + ind;
                
                copy_bytes(&program->machine.regs[srcreg].data._u64,
                    &call->regs[ind].data._u64,
                    program->machine.regs[srcreg].bytes);
                call->regs[ind].bytes = program->machine.regs[bci->dst_reg].bytes;
                call->regs[ind].type = program->machine.regs[bci->dst_reg].type;
            }
            new_call_register = call;
            break;
        }
        case BC_CALL_PROCEDURE: {
            // Assume that the arguments (maybe leave space for the return value) are in the stack
            // save the current stack pointer, set a new one for the function
            u8 *old_stack_base = program->bss.stack_base;
            u8 *old_stack_pointer = program->bss.stack_pointer;
            program->bss.stack_base = program->bss.stack_pointer;
            bc_call_register *old_current = current_call_register;
            current_call_register = new_call_register;
            new_call_register = nullptr;

            FunctionDefinitionAST *fundef = (FunctionDefinitionAST *)bci->big_const;

            if (fundef->declaration->isForeign) {
                // we need to do special work for foreign functions, dyncall
                assert(fundef->declaration->func_ptr);
                callExternalFunction(fundef, bci);
            } else {
                assert(fundef->bc_function);
                run_bc_function(fundef->bc_function);
                if (bci->dst_reg != -1) {
                    copy_bytes(old_stack_pointer,
                        (u8 *)&program->machine.regs[bci->dst_reg], 
                        bci->dst_type_bytes);
                    program->machine.regs[bci->dst_reg].bytes = bci->dst_type_bytes;
                    program->machine.regs[bci->dst_reg].type = get_regtype_from_type(fundef->declaration);
                }
            }

            current_call_register = old_current;
            program->bss.stack_base = old_stack_base;
            program->bss.stack_pointer = old_stack_pointer;
            break;
        }
        case BC_RETURN: {
            // end function execution right here. 
            // In the future, do postamble work such as defer
            return; 
        }
        default:
            assert(!"Unknown Instruction type");
        }
    }
}

void bytecode_runner::run_preamble()
{
    run_bc_function(&program->preamble_function);
}

ExpressionAST * bytecode_runner::run_directive(RunDirectiveAST * run)
{
    return nullptr;
}

void bytecode_runner::callExternalFunction(FunctionDefinitionAST * fundef, BCI * bci)
{
    DCCallVM *vm;
    if (!CallVM) {
        CallVM = dcNewCallVM(4096);
        dcMode((DCCallVM *)CallVM, DC_CALL_C_DEFAULT);
    }
    vm = (DCCallVM *)CallVM;
    dcReset(vm);

    for (auto arg : fundef->declaration->arguments) {

    }
    
}

