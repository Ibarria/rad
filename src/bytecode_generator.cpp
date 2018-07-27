#include "bytecode_generator.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <dyncall.h>
#include <dynload.h>

#include "Interpreter.h"

const u64 stack_size = 10 * 1024;
// Function implemented in Parser.cpp, useful to create correct AST for #run
void copyASTinfo(BaseAST *src, BaseAST *dst);

template <class T> const T& max(const T& a, const T& b) {
    return (a<b) ? b : a;     
}

union __xxx_to_u64 {
    f64 f;
    f32 d;
    s64 s;
    u64 u;
    void *p;
};

static u64 straight_convert(f32 f) {
    __xxx_to_u64 x = { 0 };
    x.d = f;
    return x.u;
}


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

static u64 upconvertUIntReg(bc_register &r)
{
    switch (r.bytes) {
    case 1: return r.data._u8;
    case 2: return r.data._u16;
    case 4: return r.data._u32;
    case 8: return r.data._u64;
    }
    assert(false);
    return 0;
}

static s64 upconvertSIntReg(bc_register &r)
{
    switch (r.bytes) {
    case 1: return r.data._s8;
    case 2: return r.data._s16;
    case 4: return r.data._s32;
    case 8: return r.data._s64;
    }
    assert(false);
    return 0;
}

static f64 upconvertFloatReg(bc_register &r)
{
    switch (r.bytes) {
    case 4: return r.data._f32;
    case 8: return r.data._f64;
    }
    assert(false);
    return 0;
}

static void downconvertUIntReg(bc_register &r, u64 v)
{
    switch (r.bytes) {
    case 1: r.data._u8 =  (u8)v;  break;
    case 2: r.data._u16 = (u16)v; break;
    case 4: r.data._u32 = (u32)v; break;
    case 8: r.data._u64 = (u64)v; break;
    default: assert(false);
    }
}

static void downconvertSIntReg(bc_register &r, s64 v)
{
    switch (r.bytes) {
    case 1: r.data._s8 =  (s8)v;  break;
    case 2: r.data._s16 = (s16)v; break;
    case 4: r.data._s32 = (s32)v; break;
    case 8: r.data._s64 = (s64)v; break;
    default: assert(false);
    }
}

static void downconvertFloatReg(bc_register &r, f64 v)
{
    switch (r.bytes) {
    case 4: r.data._f32 = (f32)v; break;
    case 8: r.data._f64 = (f64)v; break;
    default: assert(false);
    }
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
    if (type->ast_type == AST_POINTER_TYPE) {
        return REGTYPE_POINTER;
    }
    assert(type && type->ast_type == AST_DIRECT_TYPE);
    auto dt = (DirectTypeAST *)type;
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
        (isFunctionDefinition(decl) || isStructDeclaration(decl))) {
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
    // This assert might not make sense on a func :: () 
    // assert(bytes > 0);
    u64 qwcount = bytes / 8;
    if (bytes % 8 != 0) qwcount++;
    assert(qwcount == (s16)qwcount);
    return (s16)qwcount;
}

static inline s16 reserveRegisters(bytecode_function *func, u64 num_regs)
{
    s16 r = func->num_regs;
    assert(num_regs < 32767);
    func->num_regs += (s16)num_regs;
    // @TODO: Possible optimization, for large structs or arrays,
    // store them in the stack and just copy here pointers to that
    return r;
}

static inline s16 reserveRegistersForSize(bytecode_function *func, u64 size_in_bytes)
{
    return reserveRegisters(func, roundToQWord(size_in_bytes));
}

/*
    This function computes how many bytes we need in the BSS for all the
    global variables we have. 
*/
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

        u64 var_size = getVariableSize(decl);

        if (var_size > 0) {
            assert(decl->specified_type);
            assert(decl->specified_type->size_in_bytes > 0);
            // It cannot be a function argument because those are stored in registers
            assert(!(decl->flags & DECL_FLAG_IS_FUNCTION_ARGUMENT));

            // the offset for global variables is the accumulated size in bytes
            decl->bc_offset = total_size;
            
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

static BytecodeInstructionOpcode getStoreOpcode(VariableDeclarationAST *decl)
{
    BytecodeInstructionOpcode opcode;
    if (isGlobal(decl)) {
        opcode = BC_STORE_TO_BSS_PLUS_CONSTANT;
    } else if (isLocal(decl)) {
        opcode = BC_STORE_TO_STACK_PLUS_CONSTANT;
    } else if (isArgument(decl)) {
        opcode = BC_STORE_TO_CALL_REGISTER;
    } else {
        assert(!"Variable without scope set!");
        opcode = BC_UNINITIALIZED;
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
        opcode = BC_UNINITIALIZED;
    }
    return opcode;
}

#define CASE_BC_OPCODE(a) case a: return #a
const char *bc_opcode_to_str(BytecodeInstructionOpcode opcode)
{
    switch(opcode)
    {
        CASE_BC_OPCODE(BC_UNINITIALIZED);
        CASE_BC_OPCODE(BC_NOP);
        CASE_BC_OPCODE(BC_ZERO_REG);
        CASE_BC_OPCODE(BC_COPY_REG);
        CASE_BC_OPCODE(BC_LOAD_BIG_CONSTANT_TO_REG);
        CASE_BC_OPCODE(BC_STORE_TO_STACK_PLUS_CONSTANT);
        CASE_BC_OPCODE(BC_STORE_TO_BSS_PLUS_CONSTANT);
        CASE_BC_OPCODE(BC_STORE_TO_MEM_PTR);
        CASE_BC_OPCODE(BC_STORE_TO_CALL_REGISTER);
        CASE_BC_OPCODE(BC_LOAD_FROM_STACK_PLUS_CONSTANT);
        CASE_BC_OPCODE(BC_LOAD_FROM_BSS_PLUS_CONSTANT);
        CASE_BC_OPCODE(BC_LOAD_FROM_CALL_REGISTER);
        CASE_BC_OPCODE(BC_LOAD_FROM_MEM_PTR);
        CASE_BC_OPCODE(BC_ADDRESS_FROM_STACK_PLUS_CONSTANT);
        CASE_BC_OPCODE(BC_ADDRESS_FROM_BSS_PLUS_CONSTANT);
        CASE_BC_OPCODE(BC_ADDRESS_FROM_CALL_REGISTER);
        CASE_BC_OPCODE(BC_CREATE_CALL_REGISTER);
        CASE_BC_OPCODE(BC_CALL_PROCEDURE);
        CASE_BC_OPCODE(BC_RETURN);
        CASE_BC_OPCODE(BC_BINARY_OPERATION);
        CASE_BC_OPCODE(BC_UNARY_OPERATION);
        CASE_BC_OPCODE(BC_INC_REG_CONST);
        CASE_BC_OPCODE(BC_RESERVE_STACK_SIZE);
        CASE_BC_OPCODE(BC_CAST);
        CASE_BC_OPCODE(BC_GOTO_CONSTANT_IF_FALSE);
        CASE_BC_OPCODE(BC_GOTO_CONSTANT_IF_TRUE);
        CASE_BC_OPCODE(BC_GOTO_CONSTANT);
        CASE_BC_OPCODE(BC_MALLOC);
        CASE_BC_OPCODE(BC_FREE);
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

static void copyLoc(BCI *bci, BaseAST *ast)
{
    bci->char_num = ast->char_num;
    bci->line_num = ast->line_num;
}

void print_instruction(BCI *inst)
{
    printf("  [%3d] op: %-35s src: %3d src2: %3d dst: %3d type: %s size: %d ",
        inst->inst_index, bc_opcode_to_str(inst->opcode), (int)inst->src_reg, (int)inst->src2_reg,
        (int)inst->dst_reg, RegTypeToStr(inst->dst_type), inst->dst_type_bytes);

    if ((inst->opcode == BC_BINARY_OPERATION) || (inst->opcode == BC_UNARY_OPERATION)) {
        printf("operator: %s\n", TokenTypeToStr((TOKEN_TYPE)inst->big_const));
    } else {
        printf("big_const: %" U64FMT "u | 0x%" U64FMT "X\n",
            inst->big_const, inst->big_const);
    }
}

void print_bc_function(bytecode_function *func)
{
    if (func->function_name) {
        printf("Function : %s ", func->function_name);
    } else {
        printf("Function <unnamed> ");
    }
    printf("local regs: %d reg arg size: %" U64FMT "u local var size: %" U64FMT "u num_instructions: %d\n",
           func->num_regs, func->bc_arg_size, func->bc_local_var_size, func->instructions.size());
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
    
    bci = create_instruction(opcode, reg, -1, -1, bc_mem_offset);
    bci->dst_type_bytes = truncate_op_size(size_in_bytes);
    bci->dst_type = regtype;
    if (size_in_bytes > 8) {
        s64 bytes = size_in_bytes;
        u64 offset_increment = 8;
        if (opcode == BC_STORE_TO_CALL_REGISTER) {
            offset_increment = 1;
        }
        u64 offset = bc_mem_offset + offset_increment;
        bytes -= 8;
        reg++;
        do {
            bci = create_instruction(opcode, reg, -1, -1, offset);
            bci->dst_type_bytes = truncate_op_size(bytes);
            bci->dst_type = regtype;
            reg++;
            bytes -= 8;
            offset += offset_increment;
        } while (bytes > 0);
    }
}

void bytecode_generator::createStoreInstruction(BytecodeInstructionOpcode opcode, s16 ptrreg, s16 datareg, u64 size_in_bytes)
{
    BCI *bci;

    bci = create_instruction(opcode, datareg, -1, ptrreg, 0);
    bci->dst_type_bytes = truncate_op_size(size_in_bytes);
    bci->dst_type = REGTYPE_UNKNOWN;
    if (size_in_bytes > 8) {
        s64 bytes = size_in_bytes;
        u64 offset = 8;
        bytes -= 8;
        datareg++;
        do {
            bci = create_instruction(opcode, datareg, -1, ptrreg, offset);
            bci->dst_type_bytes = truncate_op_size(bytes);
            bci->dst_type = REGTYPE_UNKNOWN;
            datareg++;
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

    createStoreInstruction(opcode, decl->bc_offset,
                           decl->specified_type->size_in_bytes, reg, regtype);
}

void bytecode_generator::createLoadInstruction(BytecodeInstructionOpcode opcode, u64 bc_mem_offset, 
                                               u64 size_in_bytes, s16 reg, RegisterType regtype)
{
    BCI *bci;

    bci = create_instruction(opcode, -1, -1, reg, bc_mem_offset);
    bci->dst_type_bytes = truncate_op_size(size_in_bytes);
    bci->dst_type = regtype;
    if (size_in_bytes > 8) {
        s64 bytes = size_in_bytes;
        u64 offset = bc_mem_offset + 8;
        bytes -= 8;
        reg++;
        do {
            bci = create_instruction(opcode, -1, -1, reg, offset);
            bci->dst_type_bytes = truncate_op_size(bytes);
            bci->dst_type = regtype;
            reg++;
            bytes -= 8;
            offset += 8;
        } while (bytes > 0);
    }
}

BCI *bytecode_generator::createAddressInstruction(VariableDeclarationAST * decl)
{
    BytecodeInstructionOpcode opcode;
    if (isGlobal(decl)) {
        opcode = BC_ADDRESS_FROM_BSS_PLUS_CONSTANT;
    } else if (isLocal(decl)) {
        opcode = BC_ADDRESS_FROM_STACK_PLUS_CONSTANT;
    } else if (isArgument(decl)) {
        opcode = BC_ADDRESS_FROM_CALL_REGISTER;
    } else {
        assert(!"Variable without scope set!");
        opcode = BC_UNINITIALIZED;
    }

    s16 reg = reserveRegister();

    auto old = current_ast;
    current_ast = decl;
    BCI *bci = create_instruction(opcode, -1, -1, reg, decl->bc_offset);
    bci->dst_type_bytes = 8;
    bci->dst_type = REGTYPE_POINTER;
    current_ast = old;

    return bci;
}

BCI *bytecode_generator::createLoadOffsetInstruction(ExpressionAST * expr)
{
    auto old = current_ast;
    BCI *ret = nullptr;
    current_ast = expr;
    switch (expr->ast_type)
    {
    case AST_STRUCT_ACCESS: {
        auto sac = (StructAccessAST *)expr;

        if (sac->next) {
            // load the current offset
            auto offset = loadConstantU64(sac->decl->bc_offset);

            // load the inner offset
            auto inner = createLoadOffsetInstruction(sac->next);
            // add them up together
            ret = create_instruction(BC_BINARY_OPERATION, offset->dst_reg, inner->dst_reg, reserveRegister(), TK_PLUS);
            ret->dst_type = REGTYPE_UINT;
            ret->dst_type_bytes = 8;
        } else {
            ret = loadConstantU64(sac->decl->bc_offset);
        }

        break;
    }
    case AST_ARRAY_ACCESS: {
        auto ac = (ArrayAccessAST *)expr;

        // @TODO: Fix this to support either static arrays, Sized arrays or dynamic ones
        // Right now only handles static arrays

        auto index_inst = computeExpression(ac->array_exp);
        BCI *bci = create_instruction(BC_CAST, index_inst->dst_reg, -1, index_inst->dst_reg, 0);
        bci->dst_type = REGTYPE_UINT;
        bci->dst_type_bytes = 8;

        // Multiply the index by the size of each array elemen
        auto array_size = loadConstantU64(ac->access_type->size_in_bytes);

        auto array_offset = create_instruction(BC_BINARY_OPERATION, index_inst->dst_reg, array_size->dst_reg, reserveRegister(), TK_STAR);
        array_offset->dst_type = REGTYPE_UINT;
        array_offset->dst_type_bytes = 8;

        ret = array_offset;

        if (ac->next) {
            auto next_offset = createLoadOffsetInstruction(ac->next);
            bci = create_instruction(BC_BINARY_OPERATION, array_offset->dst_reg, next_offset->dst_reg, reserveRegister(), TK_STAR);
            bci->dst_type = REGTYPE_UINT;
            bci->dst_type_bytes = 8;

            ret = bci;
        } 
        break;
    }
    case AST_IDENTIFIER: {
        auto id = (IdentifierAST *)expr;
        loadConstantU64(id->decl->bc_offset);
        break;
    }
    default:
        assert(!"This function can only be called with something that can yield a declaration");
    }
    current_ast = old;
    return ret;
}

BCI * bytecode_generator::createNopInstruction(BaseAST *ast)
{
    auto old = current_ast;
    current_ast = ast;
    BCI *bci = create_instruction(BC_NOP, -1, -1, -1, 0);
    current_ast = old;
    return bci;
}

void bytecode_generator::createLoadInstruction(VariableDeclarationAST *decl, s16 reg)
{
    BytecodeInstructionOpcode opcode = getLoadOpcode(decl);

    RegisterType regtype = get_regtype_from_type(decl->specified_type);

    createLoadInstruction(opcode, decl->bc_offset,
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
    #if defined(PLATFORM_WINDOWS)
    lib_name[l - 3] = 'd'; lib_name[l - 2] = 'l'; lib_name[l - 1] = 'l';
    #elif defined(PLATFORM_LINUX)
    lib_name[l - 3] = 's'; lib_name[l - 2] = 'o'; lib_name[l - 1] = '\0';
    #elif defined(PLATFORM_MACOS)
    lib_name[l - 3] = 'd'; lib_name[l - 2] = 'y'; lib_name[l - 1] = 'l'; lib_name[l] = 'i'; lib_name[l + 1] = 'b';	
    #endif

    TextType intern_lib_name = CreateTextType(pool, lib_name);

    for (auto lib : program->external_libs) {
        if (lib->name == intern_lib_name) {
            return lib;
        }
    }
    // If we are here, we need to load the library
    external_library *lib = new (pool) external_library;
    lib->name = intern_lib_name;
    lib->dll = dlLoadLibrary(intern_lib_name);
    if (lib->dll == nullptr) {
        printf("Could not find library: %s\n", intern_lib_name);
    }
    assert(lib->dll != nullptr);
    program->external_libs.push_back(lib);
    return lib;
}

static u64 instruction_serial = 100;

s16 bytecode_generator::reserveRegisters(s16 num)
{
    return ::reserveRegisters(current_function, num);
}

BCI * bytecode_generator::create_instruction(BytecodeInstructionOpcode opcode, s16 src_reg, s16 src2_reg, s16 dst_reg, u64 big_const)
{
    BCI *bci = new (pool) BCI;
    bci->opcode = opcode;
    bci->src_reg = src_reg;
    bci->src2_reg = src2_reg;
    bci->dst_reg = dst_reg;
    bci->big_const = big_const;
    bci->s = instruction_serial++;

    // for debugging purposes
    assert(current_ast != nullptr);
    copyLoc(bci, current_ast);
    issue_instruction(bci);
    return bci;
}

void bytecode_generator::issue_instruction(BCI * bci)
{
    bci->inst_index = current_function->instructions.size();
    current_function->instructions.push_back(bci);
}

void bytecode_generator::issueReserveStackSpace(u64 size)
{
    BCI *bci = create_instruction(BC_RESERVE_STACK_SIZE, 0, 0, 0, size);
    bci->dst_type_bytes = 8;
}

watermark bytecode_generator::getWatermark()
{
    watermark wm;
    assert(current_function);
    wm.func = current_function;
    wm.inst_index = current_function->instructions.size();
    wm.regs = current_function->num_regs;
    return wm;
}

void bytecode_generator::resetWatermark(watermark wm)
{
    assert(current_function == wm.func);
    current_function->instructions.reset(wm.inst_index);
    current_function->num_regs = wm.regs;
}

void bytecode_generator::startGlobalCompile(FileAST * root)
{
    if (this->program != nullptr) return;

    bytecode_program *bp = new (pool) bytecode_program;
    this->program = bp;

    // First step, have space in the bss for the global variables (non functions)
    u64 bss_size = getScopeVariablesSize(&root->global_scope);
    // ensure we get page aligned memory chunks
    bss_size = roundToPage(bss_size, 4 * 1024);

    bp->bss.initMem((u8 *)pool->alloc(bss_size + stack_size),
        bss_size, stack_size);
}

void bytecode_generator::compileAllFunctions(FileAST * root)
{
    CPU_SAMPLE("compile bytecode");

    initializeGlobalFunctions(&root->global_scope);
}

bool bytecode_generator::validateAllFunctions()
{
    for (auto func : program->functions) {
        if (func->missing_run_directives > 0) {
            interp->Error(nullptr, "Function %s had dependent #run directives that could not be evaluated\n",
                func->function_name);
            return false;
        }
    }
    return true;
}

void bytecode_generator::generate_function(TextType name, FunctionDefinitionAST * fundef)
{
    fundef->being_generated = true;
    if (fundef->declaration->isForeign) {
        assert(fundef->declaration->func_ptr == nullptr);

        external_library *exlib = findOrLoadLibrary(fundef->declaration->filename);
        if (exlib == nullptr) {
            printf("Could not find compiler library: %s\n", fundef->declaration->filename);
            exit(1);
        }
        fundef->declaration->func_ptr = dlFindSymbol((DLLib *)exlib->dll, fundef->var_decl->varname);
        if (fundef->declaration->func_ptr == nullptr) {
            printf("Could not load function %s from compiler library: %s\n", 
                fundef->var_decl->varname, fundef->declaration->filename);
            exit(1);
        }
                
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
        u32 reg_needed = roundToQWord(fundecl->return_type->size_in_bytes);
        current_function->bc_arg_size += reg_needed;

        // We do not support return types more than 16 bytes (for strings)
        // This could be improved by converting return values into a pointer input like clang
        assert(reg_needed <= 2);
    }
    for (auto arg: fundecl->arguments) {
        // compute the size and offset for the arguments
        // For arguments, bc_offset is the register index in the calling_register
        assert(arg->specified_type);
        u64 reg_needed = roundToQWord(getVariableSize(arg));
        arg->bc_offset = current_function->bc_arg_size;
        current_function->bc_arg_size += reg_needed;
    }
    
    // Creating a function is the same as processing its statementBlock
    generate_statement_block(fundef->function_body);
    
    current_function = old_current;
    fundef->being_generated = false;

    if (!interp->success) {
        // This wastes all the instructions on the function so far. 
        // We try to run #run directives in place, but it might fail
        return;
    }

    if (!strcmp(name, "main") && (fundef->scope->parent == nullptr)) {
        program->start_function = func;
    }
    assert(fundef->bc_function == nullptr);
    fundef->bc_function = func;
    program->functions.push_back(func);
}

void bytecode_generator::generate_statement(StatementAST *stmt)
{
    auto old = current_ast;
    current_ast = stmt;
    switch (stmt->ast_type) {
    case AST_VARIABLE_DECLARATION: {
        auto decl = (VariableDeclarationAST *)stmt;
        u64 var_size = getVariableSize(decl);
        if (var_size > 0) {
            // Possible optimization: have stack space in a per block basis
            // We might need this for defers too. 
            // reserve space and figure out where it is
            decl->bc_offset = current_function->bc_local_var_size;
            current_function->bc_local_var_size += var_size;
            issueReserveStackSpace(var_size);
        }
        initializeVariable(decl);
        break;
    }
    case AST_ASSIGNMENT: {
        auto assign = (AssignmentAST *)stmt;
        auto rhs = computeExpression(assign->rhs);

        if (assign->lhs->ast_type == AST_IDENTIFIER) {
            auto iden = (IdentifierAST *)assign->lhs;
            auto id_address = computeAddress(iden);
            createStoreInstruction(BC_STORE_TO_MEM_PTR, id_address->dst_reg, rhs->dst_reg, iden->expr_type->size_in_bytes);
        }
        else if (assign->lhs->ast_type == AST_UNARY_OPERATION) {
            auto unop = (UnaryOperationAST *)assign->lhs;
            assert(unop->op == TK_LSHIFT);
            // LHS LSHIFT is always a pointer, load the pointer (which is the address where)
            // we want to store later
            auto ptr_reg = computeExpression(unop->expr);            
            createStoreInstruction(BC_STORE_TO_MEM_PTR, ptr_reg->dst_reg, rhs->dst_reg, unop->expr_type->size_in_bytes);
        }
        else {
            assert(!"We do not support anything else for an lvalue");
        }

        break;
    }
    case AST_FUNCTION_CALL: {
        auto funcall = (FunctionCallAST *)stmt;
        // on statements we do not care about return values

        // Recursive bytecode generation, if needed. Being_generated is here for recursive calls, or circular calls
        if (funcall->fundef->bc_function == nullptr && !funcall->fundef->being_generated) {
            generate_function(funcall->function_name, funcall->fundef);
            assert(funcall->fundef->bc_function);
        }

        compute_function_call(funcall);
        break;
    }
    case AST_RETURN_STATEMENT: {
        auto ret_stmt = (ReturnStatementAST *)stmt;
        auto ret_inst = computeExpression(ret_stmt->ret);
        createStoreInstruction(BC_STORE_TO_CALL_REGISTER, 0, ret_stmt->ret->expr_type->size_in_bytes,
            ret_inst->dst_reg, get_regtype_from_type(ret_stmt->ret->expr_type));
        create_instruction(BC_RETURN, -1, -1, -1, 0);
        break;
    }
    case AST_STATEMENT_BLOCK: {
        auto inner_block = (StatementBlockAST *)stmt;
        generate_statement_block(inner_block);
        break;
    }
    case AST_IF_STATEMENT: {
        auto ifst = (IfStatementAST *)stmt;
        auto if_inst = computeExpression(ifst->condition);
        BCI *if_cond_bci = create_instruction(BC_GOTO_CONSTANT_IF_FALSE, if_inst->dst_reg, -1, -1, -1);
        generate_statement(ifst->then_branch);

        BCI *else_cond_bci = nullptr;
        if (ifst->else_branch != nullptr) {
            else_cond_bci = create_instruction(BC_GOTO_CONSTANT, -1, -1, -1, -1);
            // We have to choose the index+1 otherwise we never end in the else clause
            if_cond_bci->big_const = else_cond_bci->inst_index+1;
            generate_statement(ifst->else_branch);
        }
        BCI *end_nop = create_instruction(BC_NOP, -1, -1, -1, -1);
        // record the jump offset for the case when we do not go into the if
        if (ifst->else_branch != nullptr) {
            else_cond_bci->big_const = end_nop->inst_index;
        } else {
            if_cond_bci->big_const = end_nop->inst_index;
        }
        break;
    }
    case AST_FOR_STATEMENT: {
        auto forst = (ForStatementAST *)stmt;
        // Allocate loop variables. This also initializes them
        for (auto decl : forst->for_scope.decls) {
            generate_statement(decl);
        }

        BCI *bci_loop_cond = nullptr;
        s32 loop_start = -1, loop_end = -1;
        // Work before the loop block
        if (forst->is_array) {
            auto atype = (ArrayTypeAST *)forst->arr->expr_type;

            auto nop = createNopInstruction(forst);
            // the loop starts at this instruction
            loop_start = nop->inst_index;

            auto array_count = computeArrayCount(forst->arr);

            // Load the value of it_index, it is either 0 (start value) or whatever comes from the loop
            auto it_index_val = computeExpression(forst->it_index);

            auto cond_val = create_instruction(BC_BINARY_OPERATION, it_index_val->dst_reg, array_count->dst_reg, reserveRegister(), TK_GT);
            cond_val->dst_type = REGTYPE_UINT;
            cond_val->dst_type_bytes = 1;

            bci_loop_cond = create_instruction(BC_GOTO_CONSTANT_IF_TRUE, cond_val->dst_reg, -1, -1, loop_end);
            // store this instruction, as we do not know the end yet

            // Load the correct value for it, before we go into the main loop
            {
                auto array_data_ptr = computeArrayDataPtr(forst->arr);

                auto array_elem_size = loadConstantU64(atype->array_of_type->size_in_bytes);

                BCI *it_new_val;
                auto it_offset = create_instruction(BC_BINARY_OPERATION, array_elem_size->dst_reg, it_index_val->dst_reg, 
                    reserveRegister(), TK_STAR);
                it_offset->dst_type = REGTYPE_UINT;
                it_offset->dst_type_bytes = 8;

                auto it_ptr = create_instruction(BC_BINARY_OPERATION, array_data_ptr->dst_reg, it_offset->dst_reg, 
                    reserveRegister(), TK_PLUS);
                it_ptr->dst_type = REGTYPE_POINTER;
                it_ptr->dst_type_bytes = 8;

                if (forst->is_it_ptr) {
                    it_new_val = it_ptr;
                } else {
                    it_new_val = derefPointer(it_ptr, 
                        get_regtype_from_type(forst->it->decl->specified_type), 
                        forst->it->decl->specified_type->size_in_bytes);
                }

                createStoreInstruction(forst->it->decl, it_new_val->dst_reg);
            }

        } else {
            // Keep the end expression (should not change) loaded
            auto end = computeExpression(forst->end);

            auto nop = createNopInstruction(forst);
            // the loop starts at this instruction
            loop_start = nop->inst_index ;

            // Now do the check if continue or not
            // load it value
            auto it_val = computeExpression(forst->it);

            // The expression TK_GT here is so the start, end are inclusive
            // There is a possible, remote problem: if the end is also the last
            // representable number, we would never complete the loop. Given we use 64 bit
            // integers, this is unlikely here
            auto cond_val = create_instruction(BC_BINARY_OPERATION, it_val->dst_reg, end->dst_reg, reserveRegister(), TK_GT);
            cond_val->dst_type = REGTYPE_UINT;
            cond_val->dst_type_bytes = 1;

            bci_loop_cond = create_instruction(BC_GOTO_CONSTANT_IF_TRUE, cond_val->dst_reg, -1, -1, loop_end);
            // store this instruction, as we do not know the end yet
        }

        // Next generate the loop itself
        generate_statement(forst->loop_block);

        // Now we need to increment the loop variables
        if (forst->is_array) {
            // Load the value of it_index
            auto it_index_val = computeExpression(forst->it_index);

            auto new_index = create_instruction(BC_INC_REG_CONST, it_index_val->dst_reg, -1, reserveRegister(), 1);
            // save the new it_index
            createStoreInstruction(forst->it_index->decl, new_index->dst_reg);
        } else {

            // load it again, in case the loop modified it (weird, but possible
            auto it_val = computeExpression(forst->it);

            // increment it by one
            auto it_inc_val = create_instruction(BC_INC_REG_CONST, it_val->dst_reg, -1, reserveRegister(), 1);
            // save the value
            createStoreInstruction(forst->it->decl, it_inc_val->dst_reg);

            auto it_index_val = computeExpression(forst->it_index);
            // increment it by one
            it_index_val = create_instruction(BC_INC_REG_CONST, it_index_val->dst_reg, -1, reserveRegister(), 1);
            // save the value
            createStoreInstruction(forst->it_index->decl, it_index_val->dst_reg);
        }

        // go back to the start of the loop, there we check the condition
        create_instruction(BC_GOTO_CONSTANT, -1, -1, -1, loop_start);

        // Add a nop sow e have a place to jump to, in case the loop is at the end of a function
        auto nop = createNopInstruction(forst);

        // now that we know where the loop ends, store the value in the jump instruction
        loop_end = nop->inst_index;
        bci_loop_cond->big_const = loop_end;


        break;
    }
    case AST_RUN_DIRECTIVE: {
        auto run = (RunDirectiveAST *)stmt;
        
        run->enclosing_func = current_function;
        // All run directives should be processed, the loop in Interpreter ensures it
        if (run->bc_function == nullptr) {
            interp->Error(run, "Detected recursive #run directive, this is not allowed\n");
            return;
        }
        // Void return have no code generated, just skip
        if (isVoidType(run->expr_type)) break;

        assert(run->new_ast);
        auto lit = (LiteralAST *)run->new_ast;

        assert(run->bci == nullptr);
        run->bci = create_load_literal_instruction(lit);
        run->reg = run->bci->dst_reg;

        if (!run->computed) {
            current_function->missing_run_directives++;
            assert(current_run != run);
            current_run->run_deps.push_back(run);
        }

        break;
    }
    default:
        assert(!"Generate Statement Block in Bytecode, unknown AST");
    }
    current_ast = old;
}

void bytecode_generator::generate_statement_block(StatementBlockAST *block)
{
    // first thing, allocate space for the variables, and process implicit
    // initialization
    for (auto stmt: block->statements) {
        generate_statement(stmt);
    }
}

void bytecode_generator::initializeGlobalVariables(Scope * scope)
{
    for (auto decl : scope->decls) {
        if (isFunctionDefinition(decl)) {
            // foreign functions need to be initialized early
            if (isFunctionForeign(decl)) initializeVariable(decl);
        } else {
            initializeVariable(decl);
        }
    }
}

void bytecode_generator::initializeGlobalVariable(VariableDeclarationAST * decl)
{
    assert(decl->flags & DECL_FLAG_IS_GLOBAL_VARIABLE);
    auto old = current_ast;
    current_ast = decl;
    current_function = &program->preamble_function;
    initializeVariable(decl);
    current_function = nullptr;
    current_ast = old;
}

void bytecode_generator::initializeGlobalFunctions(Scope * scope)
{
    for (auto decl : scope->decls) {
        if (isFunctionDefinition(decl) && !isFunctionForeign(decl)) {
            initializeVariable(decl);
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
    if (decl->flags & DECL_FLAG_HAS_BEEN_BT_GEN) {
        return;
    }

    if (!decl->definition) {
        decl->flags |= DECL_FLAG_HAS_BEEN_BT_GEN;
        return; // nothing to do for this var
    // @TODO: initialize it to 0 by default
    }
    
    if (decl->definition->ast_type == AST_FUNCTION_DEFINITION) {
        // Do not generate functions when doing only global vars
        auto fundef = (FunctionDefinitionAST *)decl->definition;
        if (fundef->bc_function != nullptr) {
            decl->flags |= DECL_FLAG_HAS_BEEN_BT_GEN;
            return;
        }
        generate_function(decl->varname, fundef);
        decl->flags |= DECL_FLAG_HAS_BEEN_BT_GEN;
        return;
    }
    if (decl->definition->ast_type == AST_STRUCT_DEFINITION) {
        // struct definitions do not cause any code to be created
        // when they are instantiated, yes
        decl->flags |= DECL_FLAG_HAS_BEEN_BT_GEN;
        return;
    }
    // @TODO: we do not support function pointers yet

    // ok, it is an expression (operation, literal (num, string) )
    // or god forbid, another variable (which means load its value
    watermark wm = getWatermark();    
    auto def = computeExpression((ExpressionAST *)decl->definition);
    if (!interp->success) {
        // in case of error, clean up
        resetWatermark(wm);
        return;
    }
    createStoreInstruction(decl, def->dst_reg);
    
    decl->flags |= DECL_FLAG_HAS_BEEN_BT_GEN;
}

BCI *bytecode_generator::create_load_literal_instruction(LiteralAST *lit)
{
    BCI *ret = nullptr;
    auto old = current_ast;
    current_ast = lit;

    switch (lit->typeAST->basic_type) {
    case BASIC_TYPE_INTEGER: {
        u64 val;
        if (lit->typeAST->isSigned) val = straight_convert(lit->_s64);
        else val = lit->_u64;
        ret = create_instruction(BC_LOAD_BIG_CONSTANT_TO_REG, -1, -1, reserveRegister(), val);
        assert(lit->typeAST->size_in_bytes < 256);
        ret->dst_type_bytes = (u8)lit->typeAST->size_in_bytes;
        ret->dst_type = (lit->typeAST->isSigned ? REGTYPE_SINT : REGTYPE_UINT);
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
        ret = create_instruction(BC_LOAD_BIG_CONSTANT_TO_REG, -1, -1, reserveRegister(), val);
        ret->dst_type_bytes = 8;
        ret->dst_type = REGTYPE_POINTER;
        ret->dst_num_reg = 2;

        val = strlen(lit->str);
        loadConstantU64(val);
        break;
    }
    case BASIC_TYPE_BOOL: {
        ret = create_instruction(BC_LOAD_BIG_CONSTANT_TO_REG, -1, -1, reserveRegister(), lit->_bool);
        assert(lit->typeAST->size_in_bytes < 256);
        ret->dst_type_bytes = (u8)lit->typeAST->size_in_bytes;
        ret->dst_type = REGTYPE_UINT;
        break;
    }
    case BASIC_TYPE_FLOATING: {
        u64 val;
        if (lit->typeAST->size_in_bytes == 8) val = straight_convert(lit->_f64);
        else {
            // this ensures the right floating point number gets added to the const
            f32 fval = (f32)lit->_f64;
            val = straight_convert(fval);
        }
        assert(lit->typeAST->size_in_bytes < 256);
        ret = create_instruction(BC_LOAD_BIG_CONSTANT_TO_REG, -1, -1, reserveRegister(), val);
        ret->dst_type_bytes = (u8)lit->typeAST->size_in_bytes;
        ret->dst_type = REGTYPE_FLOAT;
        break;
    }
    default:
        assert(false);
    }
    current_ast = old;
    return ret;
}


BCI *bytecode_generator::computeExpression(ExpressionAST * expr)
{
//    s16 reg = reserveRegistersForSize(current_function, expr->expr_type->size_in_bytes);
    auto old = current_ast;
    current_ast = expr;
    BCI *ret = nullptr;

    switch (expr->ast_type) {
    case AST_LITERAL: {
        auto lit = (LiteralAST *)expr;
        ret = create_load_literal_instruction(lit);
        break;
    }
    case AST_UNARY_OPERATION: {
        auto unop = (UnaryOperationAST *)expr;
        TypeAST *type = unop->expr->expr_type;

        switch (unop->op) {
        case TK_PLUS: {
            // Very simple, do practically nothing
            ret = computeExpression(unop->expr);
            current_ast = old;
            // And nothing else to do
            return ret;
        }
        case TK_MINUS: {
            ret = computeExpression(unop->expr);
            break;
        }
        case TK_BANG: {
            ret = computeExpression(unop->expr);
            break;
        }
        case TK_STAR: {
            ret = computeAddress(unop->expr);
            current_ast = old;
            // There is nothing else to be done, * expr is just the address of expression
            return ret;
        }
        case TK_LSHIFT: {
            // For pointer types, the right answer is just the pointer
            if (isTypePointer(unop->expr->expr_type)) {
                ret = computeExpression(unop->expr);
            } else {
                assert(!"I am not sure we should ever be here...");
                // computeAddressIntoRegister(unop->expr, regexp);
            }
            break;
        }
        }

        // All other unary operators do behave normally, included the dereference op

        BCI *bci = create_instruction(BC_UNARY_OPERATION, ret->dst_reg, -1, reserveRegister(), unop->op);
        assert(unop->expr_type->size_in_bytes < 256);
        bci->dst_type_bytes = (u8)unop->expr_type->size_in_bytes;
        bci->dst_type = get_regtype_from_type(unop->expr_type);
        ret = bci;
        break;
    }
    case AST_BINARY_OPERATION: {
        auto binop = (BinaryOperationAST *)expr;
        // Basic idea: 
        // Do recursive bytecode for op1, op2 in some alloc registers
        // then convert the opcode into some bytecode call (maybe just 1 inst)
        // operate on the 2 regs, write into the result
        TypeAST *lhsType = binop->lhs->expr_type;
        TypeAST *rhsType = binop->rhs->expr_type;
        auto reglhs = computeExpression(binop->lhs);
        // @Optimization: do shortcircuit? YES, MUST HAVE. But here?
        auto regrhs = computeExpression(binop->rhs);
        BCI *bci = create_instruction(BC_BINARY_OPERATION, reglhs->dst_reg, regrhs->dst_reg, reserveRegister(), binop->op);
        assert(expr->expr_type->size_in_bytes < 256);
        bci->dst_type_bytes = (u8)expr->expr_type->size_in_bytes;
        bci->dst_type = get_regtype_from_type(expr->expr_type);
        ret = bci;
        break;
    }
    case AST_ASSIGNMENT: {
        assert(!"Assignment expression bytecode not allowed");
        break;
    }
    case AST_FUNCTION_CALL: {
        auto funcall = (FunctionCallAST *)expr;

        // Recursive bytecode generation, if needed. Being_generated is here for recursive calls, or circular calls
        if (funcall->fundef->bc_function == nullptr && !funcall->fundef->being_generated) {
            generate_function(funcall->function_name, funcall->fundef);
            assert(funcall->fundef->bc_function);
        }
        ret = compute_function_call(funcall);
        break;
    }
    case AST_IDENTIFIER: {
        auto id = (IdentifierAST *)expr;
        assert(id->decl);
        BCI *bci = nullptr;

        if (isTypePointer(id->expr_type) || (id->expr_type->ast_type == AST_DIRECT_TYPE)) {
            auto id_pointer = computeAddress(id);

            // dereference the pointer to get the expression value
            ret = derefPointer(id_pointer, get_regtype_from_type(id->expr_type), (u8)id->expr_type->size_in_bytes);
            assert(id->expr_type->size_in_bytes < 256);
        } else if (id->expr_type->ast_type == AST_STRUCT_TYPE) {
            assert(!"We should never be here as the interpreter should have replaced this");
        } else if (id->expr_type->ast_type == AST_ARRAY_TYPE) {
            auto arType = (ArrayTypeAST *)id->expr_type;
            // We support arrays, first we need the data type, then the count (for SIZED, DYNAMIC) 
            // and then reserved_size (DYNAMIC only)
            s16 array_reg = reserveRegisters(3);
            auto array_addr = computeAddress(id);

            if (isStaticArray(arType)) {
                // The address of `id` is the first field, the .data pointer already
                bci = create_instruction(BC_COPY_REG, array_addr->dst_reg, -1, array_reg, -1);
                bci->dst_type_bytes = 8; // this is a pointer
                bci->dst_type = REGTYPE_POINTER;
                // now array_reg contains the array.data pointer
            } else {
                // For other array types, we need a dereference to get to .data
                derefPointer(array_addr, REGTYPE_POINTER, 8, array_reg);
                // now array_reg contains the array.data pointer
            }

            // ret will point to the first register, and by default there are 2 being returned
            ret = bci;
            ret->dst_num_reg = 2;

            // Load the constant 8, the size of a pointer as we move through the array
            auto num_8 = loadConstantU64(8);

            BCI *count_ptr;

            if (isSizedArray(arType) || isDynamicArray(arType)) {
                // create a pointer to array.count
                count_ptr = create_instruction(BC_BINARY_OPERATION, array_addr->dst_reg, num_8->dst_reg, reserveRegister(), TK_PLUS);
                count_ptr->dst_type_bytes = 8;
                count_ptr->dst_type = REGTYPE_POINTER;

                // Now dereference the pointer to get the result
                derefPointer(count_ptr, REGTYPE_UINT, 8, array_reg + 1);
                // now array_reg+1 contains the array.count value
            }

            if (isDynamicArray(arType)) {
                // create a pointer to array.reserved_size
                auto rsize_ptr = create_instruction(BC_BINARY_OPERATION, count_ptr->dst_reg, num_8->dst_reg, reserveRegister(), TK_PLUS);
                rsize_ptr->dst_type_bytes = 8;
                rsize_ptr->dst_type = REGTYPE_POINTER;

                // Now dereference the pointer to get the result
                derefPointer(rsize_ptr, REGTYPE_UINT, 8, array_reg + 2);
                // now array_reg+2 contains the array.reserved_size value

                // For Dynamic arrays we return 3 registers
                ret->dst_num_reg = 3;
            }
        } else {
            assert(!"We should never be here, we better handle all cases!");
        }

        break;
    }
    case AST_STRUCT_ACCESS:
    case AST_ARRAY_ACCESS:
        assert(!"These two AST types should never be here, only as a subset of Identifier");
        break;
    case AST_RUN_DIRECTIVE: {
        auto run = (RunDirectiveAST *)expr;
        run->enclosing_func = current_function;

        assert(run->new_ast->ast_type == AST_LITERAL);
        assert(run->new_ast);
        auto lit = (LiteralAST *)run->new_ast;

        assert(run->bci == nullptr);
        run->bci = create_load_literal_instruction(lit);
        run->reg = run->bci->dst_reg;

        if (!run->computed) {
            current_function->missing_run_directives++;
            assert(current_run != run);
            if (current_run) current_run->run_deps.push_back(run);
        }
        ret = run->bci;
        break;
    }
    case AST_NEW: {
        auto nast = (NewAllocAST *)expr;
        BCI *bci = create_instruction(BC_MALLOC, -1, -1, reserveRegister(), nast->type->size_in_bytes);
        bci->dst_type_bytes = 8;
        bci->dst_type = get_regtype_from_type(nast->expr_type);
        ret = bci;
        break;
    }
    case AST_CAST: {
        auto cast = (CastAST *)expr;
        auto expr_reg = computeExpression(cast->expr);

        // And now we need to convert / Copy what is needed
        if (cast->dstType->ast_type == AST_ARRAY_TYPE) {
            auto dstType = (ArrayTypeAST *)cast->dstType;
            if (dstType->array_type != ArrayTypeAST::SIZED_ARRAY) {
                assert(!"Unsupported cast");
                exit(-1);
            }
            assert(cast->srcType->ast_type == AST_ARRAY_TYPE);
            auto srcType = (ArrayTypeAST *)cast->srcType;
            switch (srcType->array_type) {
            case ArrayTypeAST::SIZED_ARRAY: {
                assert(!"We should never be here, no need to convert from sized to sized");
                break;
            }
            case ArrayTypeAST::DYNAMIC_ARRAY: {
                // The computed expression already contains all the items we need (contains extra, though)
                ret = expr_reg;
                // this lowers the number of registers we keep track
                ret->dst_num_reg = 2;
                break;
            }
            case ArrayTypeAST::STATIC_ARRAY: {
                // here the count is implicit, we have to output it from computed value
                // this copies the data pointer
                ret = create_instruction(BC_COPY_REG, expr_reg->dst_reg, -1, reserveRegister(), -1);
                // this adds the count from the type (assume this call will reserve a register contiguous to our previous one)
                loadConstantU64(srcType->num_elems);

                ret->dst_num_reg = 2;
                break;
            }
            default:
                assert(!"We should never be here, unknown array type");
            }
        } else {
            assert(!"Unsupported cast");
        }
        break;
    }
    case AST_NULL_PTR: {
        ret = loadConstantU64(0);
        ret->dst_type = REGTYPE_POINTER;
        break;
    }
    default:
        assert(!"Unknown expression AST for bytecode");
    }
    current_ast = old;
    return ret;
}

BCI *bytecode_generator::computeArrayDataPtr(IdentifierAST * array)
{
    assert(array->expr_type->ast_type == AST_ARRAY_TYPE);
    auto arType = (ArrayTypeAST *)array->expr_type;

    auto array_inst = computeAddress(array);

    if (isStaticArray(arType)) {
        // The address of `id` is the first field, the .data pointer already
        return array_inst;
    } else {
        // For other array types, we need a dereference to get to .data
        return derefPointer(array_inst, REGTYPE_POINTER);
    }
}

BCI *bytecode_generator::computeArrayCount(IdentifierAST * array)
{
    assert(array->expr_type->ast_type == AST_ARRAY_TYPE);
    auto arType = (ArrayTypeAST *)array->expr_type;

    if (isStaticArray(arType)) {
        return loadConstantU64(arType->num_elems);
    }

    auto array_ptr = computeAddress(array);

    // For other array types, we need a dereference to get to .data
    // Increment the pointer by 8 bytes to move the pointer to count
    auto count_ptr = create_instruction(BC_INC_REG_CONST, array_ptr->dst_reg, -1, reserveRegister(), 8);
    count_ptr->dst_type_bytes = 8; // this is a pointer
    count_ptr->dst_type = REGTYPE_POINTER;

    // And now dereference the pointer to get the count
    auto count_val = derefPointer(count_ptr, REGTYPE_POINTER);

    return count_val;
}

BCI *bytecode_generator::computeAddress(ExpressionAST * expr)
{
    auto old = current_ast;
    current_ast = expr;
    BCI *ret = nullptr;

    switch (expr->ast_type) {
    case AST_LITERAL: {
        assert(!"A literal does not have an address!");
        break;
    }
    case AST_IDENTIFIER: {
        auto id = (IdentifierAST *)expr;
        assert(id->decl);

        if (!id->next) {
            ret = createAddressInstruction(id->decl);
        } else {

            s16 offset_reg = reserveRegistersForSize(current_function, 8);
            BCI *id_ptr = createAddressInstruction(id->decl);

            // if what we access is an Array of Sized or Dynamic, the Address is just the data pointer,
            // we have to do a de-reference here, but only for array access (a.data[x] , not a.count)

            if ((id->decl->specified_type->ast_type == AST_ARRAY_TYPE) &&
                (id->next->ast_type == AST_ARRAY_ACCESS)) {
                auto at = (ArrayTypeAST *)id->decl->specified_type;
                if (isSizedArray(at) || isDynamicArray(at)) {
                    // update the register with the actual pointer to data
                    id_ptr = derefPointer(id_ptr, REGTYPE_POINTER);
                }
            }

            auto offset = createLoadOffsetInstruction(id->next);

            BCI *bci = create_instruction(BC_BINARY_OPERATION, id_ptr->dst_reg, offset->dst_reg, reserveRegister(), TK_PLUS);
            bci->dst_type_bytes = 8;
            bci->dst_type = REGTYPE_POINTER;

            ret = bci;
        }
        break;
    }
    case AST_UNARY_OPERATION: {
        auto unop = (UnaryOperationAST *)expr;
        TypeAST *type = unop->expr->expr_type;

        assert(unop->op == TK_STAR);
        ret = computeAddress(unop->expr);

        break;
    }
    case AST_BINARY_OPERATION: {
        assert(!"A binary operation does not have an address!");
        break;
    }
    case AST_FUNCTION_CALL: {
        assert(!"A function call does not have an address!");
        break;
    }
    default:
        assert(!"Unknown expression AST for bytecode");
    }

    current_ast = old;
    return ret;
}


BCI *bytecode_generator::compute_function_call(FunctionCallAST *funcall)
{
    auto old = current_ast;
    auto fundecl = funcall->fundef->declaration;
    u64 argument_qwords = 0;
    // How many registers do we need for the return value?
    s16 return_regs = -1;
    current_ast = funcall;

    // @TODO: expand this when we support implicit arguments
    
    // For now, we only support calling a function with as many arguments as declared
    // Relax this part... 
    // assert(funcall->args.size() == fundecl->arguments.size());

    if (!isVoidType(fundecl->return_type)) {
        return_regs = roundToQWord(fundecl->return_type->size_in_bytes);
        argument_qwords += return_regs;
    }

    for (u32 index = 0; index < funcall->args.size(); index++) {
        auto arg_expr = funcall->args[index];
        // Here we assume that the type is the same on arg_expr and arg_decl
        argument_qwords += roundToQWord(arg_expr->expr_type->size_in_bytes);
    }

    assert(argument_qwords < 32767);
    // Now we reserve enough registers for all the args
    s16 reg = reserveRegisters((s16)argument_qwords);
    s16 offset = 0;
    if (!isVoidType(fundecl->return_type)) {
        offset += roundToQWord(fundecl->return_type->size_in_bytes);
        assert(fundecl->return_type->size_in_bytes <= 8);
        // Commenting this code as it only zero initializes the return register and nothing else
        /*
        for (s16 ind = 0; ind < offset; ind++) {
            BCI *bci = create_instruction(BC_ZERO_REG, -1, -1, reg+ind, 0);
            bci->dst_type = REGTYPE_UINT;
            bci->dst_type_bytes = 8;
        }
        */
    }

    for (u32 index = 0; index < funcall->args.size(); index++) {
        auto arg_expr = funcall->args[index];

        auto arg_inst = computeExpression(arg_expr); 
        u32 num_regs = 0;
        do {
            auto copy = create_instruction(BC_COPY_REG, arg_inst->dst_reg+num_regs, -1, reg + offset + num_regs, -1);
            num_regs++;
        } while (num_regs < arg_inst->dst_num_reg);

        // advance the offset for the next expression to write
        offset += roundToQWord(arg_expr->expr_type->size_in_bytes);
    }

    // We compute and pass the total bytes for the arguments in order to handle 
    // foreign functions like printf, where we need to know how to load the stack
    // At this point, the registers from reg until the argument_bytes have 

    // This instruction will tell the runner what registers to use when calling the function
    BCI *bci = create_instruction(BC_CREATE_CALL_REGISTER, reg, -1, return_regs, argument_qwords);

    // When a procedure call does not return a value, the call to BC_CALL_PROCEDURE needs a -1
    // otherwise it accesses wrong memory
    s16 reg_return = -1;
    if (return_regs != -1) {
        reg_return = reserveRegister();
        assert(funcall->expr_type->size_in_bytes <= 8);
    }
    // And now we actually call the function
    bci = create_instruction(BC_CALL_PROCEDURE, -1, return_regs, reg_return, straight_convert(funcall->fundef));
    assert(fundecl->return_type->size_in_bytes < 256);
    bci->dst_type_bytes = (u8)fundecl->return_type->size_in_bytes;
    current_ast = old;
    return bci;
}

BCI * bytecode_generator::loadConstantU64(u64 val)
{
    BCI *bci = create_instruction(BC_LOAD_BIG_CONSTANT_TO_REG, -1, -1, reserveRegister(), val);
    bci->dst_type_bytes = 8;
    bci->dst_type = REGTYPE_UINT;
    return bci;
}

BCI * bytecode_generator::derefPointer(BCI * ptr_inst, RegisterType type, u8 bytes)
{
    return derefPointer(ptr_inst, type, bytes, reserveRegister());
}

BCI * bytecode_generator::derefPointer(BCI * ptr_inst, RegisterType type, u8 bytes, s16 dst_reg)
{
    auto bci = create_instruction(BC_UNARY_OPERATION, ptr_inst->dst_reg, -1, dst_reg, TK_LSHIFT);
    bci->dst_type_bytes = bytes;
    bci->dst_type = type;

    return bci;
}


void bc_base_memory::initMem(u8 * basemem, u64 bss_size, u64 stack_sz)
{
    mem = basemem;
    alloc_size = bss_size + stack_sz;
    used_size = 0;
    stack_start = mem + bss_size;
    stack_base = stack_pointer = stack_start;
    stack_size = stack_sz;
}

void bytecode_runner::run_bc_function(bytecode_function * func)
{
    u32 inst_index = 0;
    bc_register *old_regs = func->regs;
    // calloc and malloc can allocate 0 bytes, but this makes some memory issues harder to debug
    if (func->num_regs > 0)
        func->regs = (bc_register *)calloc(func->num_regs, sizeof(bc_register));
    else
        func->regs = nullptr;

    while (inst_index < func->instructions.size()) {
        auto bci = func->instructions[inst_index];
        auto &dstreg = func->regs[bci->dst_reg];
        auto &srcreg = func->regs[bci->src_reg];
        auto &src2reg = func->regs[bci->src2_reg];

        switch (bci->opcode) {
        case BC_NOP: 
            break;
        case BC_ZERO_REG: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));
            dstreg.data._u64 = 0;
            dstreg.bytes = bci->dst_type_bytes;
            dstreg.type = bci->dst_type;
            break;
        }
        case BC_COPY_REG: {
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));
            assert((bci->src_reg < func->num_regs) && (bci->src_reg >= 0));
            memcpy(&dstreg, &srcreg, sizeof(dstreg));
            break;
        }
        case BC_LOAD_BIG_CONSTANT_TO_REG: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));
            copy_bytes(&bci->big_const, &dstreg.data._u64, bci->dst_type_bytes);
            dstreg.bytes = bci->dst_type_bytes;
            dstreg.type = bci->dst_type;
            break;
        }
        case BC_STORE_TO_STACK_PLUS_CONSTANT: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert(program->bss.stack_start + program->bss.stack_size > program->bss.stack_base + bci->big_const);
            assert((bci->src_reg < func->num_regs) && (bci->src_reg >= 0));
            copy_bytes((u8*)(&srcreg.data._u64),
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
            assert((bci->src_reg < func->num_regs) && (bci->src_reg >= 0));
            copy_bytes((u8 *)&srcreg.data._u64,
                &program->bss.mem[bci->big_const],
                bci->dst_type_bytes);
            break;
        }
        case BC_STORE_TO_MEM_PTR: {
            assert(dstreg.type == REGTYPE_POINTER);
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));
            assert((bci->src_reg < func->num_regs) && (bci->src_reg >= 0));
            u8 * ptr = (u8 *)dstreg.data._ptr;
            ptr += bci->big_const;
            copy_bytes((u8 *)&srcreg.data._u64, ptr, bci->dst_type_bytes);
            break;
        }
        case BC_STORE_TO_CALL_REGISTER: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert(bci->big_const < current_call_register->num_regs);
            assert((bci->src_reg < func->num_regs) && (bci->src_reg >= 0));
            copy_bytes(&srcreg.data._u64,
                &current_call_register->data[bci->big_const]._u64,                
                bci->dst_type_bytes);
            break;
        }
        case BC_LOAD_FROM_MEM_PTR: {
            assert((bci->src_reg < func->num_regs) && (bci->src_reg >= 0));
            assert(srcreg.type == REGTYPE_POINTER);
            u8 * ptr = (u8 *)srcreg.data._ptr;
            ptr += bci->big_const;
            copy_bytes(ptr, (u8 *)&srcreg.data._u64, bci->dst_type_bytes);
            break;
        }
        case BC_LOAD_FROM_STACK_PLUS_CONSTANT: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));
            assert(program->bss.stack_start + program->bss.stack_size > program->bss.stack_base + bci->big_const);
            copy_bytes(&program->bss.stack_base[bci->big_const],
                (u8 *)&dstreg.data._u64,
                bci->dst_type_bytes);
            dstreg.bytes = bci->dst_type_bytes;
            dstreg.type = bci->dst_type;
            break;
        }
        case BC_LOAD_FROM_BSS_PLUS_CONSTANT: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert(bci->big_const < program->bss.alloc_size);
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));
            copy_bytes(&program->bss.mem[bci->big_const],
                (u8 *)&dstreg.data._u64, 
                bci->dst_type_bytes);
            dstreg.bytes = bci->dst_type_bytes;
            dstreg.type = bci->dst_type;
            break;
        }
        case BC_LOAD_FROM_CALL_REGISTER: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert(current_call_register);
            assert(current_call_register->num_regs > bci->big_const);
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));
            copy_bytes(&current_call_register->data[bci->big_const]._u64,
                &dstreg.data._u64,
                bci->dst_type_bytes);
            dstreg.bytes = bci->dst_type_bytes;
            dstreg.type = bci->dst_type;
            break;
        }
        case BC_ADDRESS_FROM_STACK_PLUS_CONSTANT: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert(program->bss.stack_start + program->bss.stack_size > program->bss.stack_base + bci->big_const);
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));

            dstreg.data._ptr = &program->bss.stack_base[bci->big_const];

            dstreg.bytes = bci->dst_type_bytes;
            dstreg.type = bci->dst_type;
            break;
        }
        case BC_ADDRESS_FROM_BSS_PLUS_CONSTANT: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert(bci->big_const < program->bss.alloc_size);
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));
            dstreg.data._ptr = &program->bss.mem[bci->big_const];

            dstreg.bytes = bci->dst_type_bytes;
            dstreg.type = bci->dst_type;
            break;
        }
        case BC_ADDRESS_FROM_CALL_REGISTER: {
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert(current_call_register);
            // Call registers are order on a per register granularity
            assert(current_call_register->num_regs > bci->big_const);
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));

            // Since we are moving to doing address + mem access for all vars, we need to allow this
            // assert(!"Taking an address from a function parameter is not allowed");
            dstreg.data._ptr = (u8 *)&current_call_register->data[bci->big_const]._u64;

            dstreg.bytes = bci->dst_type_bytes;
            dstreg.type = bci->dst_type;
            break;
        }
        case BC_RESERVE_STACK_SIZE: {
            program->bss.stack_pointer += bci->big_const;
            assert(program->bss.stack_pointer < program->bss.stack_start + program->bss.stack_size);
            break;
        }
        case BC_INC_REG_CONST: {
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));
            assert((bci->src_reg < func->num_regs) && (bci->src_reg >= 0));
            assert(srcreg.type != REGTYPE_UNKNOWN);
            if (srcreg.type == REGTYPE_UINT) {                          
                if (srcreg.bytes == 1) {                               
                    dstreg.data._u8 = srcreg.data._u8 +  (u8)bci->big_const;   
                } else if (srcreg.bytes == 2) {                        
                    dstreg.data._u16 = srcreg.data._u16 + (u16)bci->big_const;
                } else if (srcreg.bytes == 4) {                        
                    dstreg.data._u32 = srcreg.data._u32 + (u32)bci->big_const;
                } else {                                            
                    assert(srcreg.bytes == 8);                         
                    dstreg.data._u64 = srcreg.data._u64 + (u64)bci->big_const;
                }                                                   
            } else if (srcreg.type == REGTYPE_SINT) {
                if (srcreg.bytes == 1) {
                    dstreg.data._s8 = srcreg.data._s8 + (s8)bci->big_const;
                } else if (srcreg.bytes == 2) {
                    dstreg.data._s16 = srcreg.data._s16 + (s16)bci->big_const;
                } else if (srcreg.bytes == 4) {
                    dstreg.data._s32 = srcreg.data._s32 + (s32)bci->big_const;
                } else {
                    assert(srcreg.bytes == 8);
                    dstreg.data._s64 = srcreg.data._s64 + (s64)bci->big_const;
                }
            } else {
                assert(!"Inrement not supported for pointers or floating point yet");
            }
            dstreg.type = srcreg.type;
            dstreg.bytes = srcreg.bytes;
            break;
        }
        case BC_BINARY_OPERATION: {
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));
            assert((bci->src_reg < func->num_regs) && (bci->src_reg >= 0));
            assert(bci->dst_type != REGTYPE_UNKNOWN);

            // at this level, types have to match. It is up to the higher levels to put casts in
            // bytecode
            // Relaxing this for now until we have casts
            //assert(srcreg.type == src2reg.type);
            //assert(srcreg.bytes == src2reg.bytes);

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
            //case TK_RSHIFT: {
            //    assert((srcreg.type == REGTYPE_UINT) || (srcreg.type == REGTYPE_SINT));
            //    if (srcreg.type == REGTYPE_UINT) {
            //        BINOP_UINT(dstreg, srcreg, src2reg, >> );
            //    } else if (srcreg.type == REGTYPE_SINT) {
            //        BINOP_SINT(dstreg, srcreg, src2reg, >> );
            //    }

            //    dstreg.type = srcreg.type;
            //    dstreg.bytes = srcreg.bytes;

            //    break;
            //}
            //case TK_LSHIFT: {
            //    assert((srcreg.type == REGTYPE_UINT) || (srcreg.type == REGTYPE_SINT));
            //    if (srcreg.type == REGTYPE_UINT) {
            //        BINOP_UINT(dstreg, srcreg, src2reg, << );
            //    } else if (srcreg.type == REGTYPE_SINT) {
            //        BINOP_SINT(dstreg, srcreg, src2reg, << );
            //    }

            //    dstreg.type = srcreg.type;
            //    dstreg.bytes = srcreg.bytes;

            //    break;
            //}
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
                    if (src2reg.type == REGTYPE_SINT) {
                        switch (src2reg.bytes) {
                        case 8:
                            dstreg.data._ptr = srcreg.data._ptr + src2reg.data._s64;
                            break;
                        case 4: 
                            dstreg.data._ptr = srcreg.data._ptr + src2reg.data._s32;
                            break;
                        case 2: 
                            dstreg.data._ptr = srcreg.data._ptr + src2reg.data._s16;
                            break;
                        case 1:
                            dstreg.data._ptr = srcreg.data._ptr + src2reg.data._s8;
                            break;
                        default:
                            assert(!"Wrong size for a register!");
                        }

                    } else if (src2reg.type == REGTYPE_UINT) {
                        switch (src2reg.bytes) {
                        case 8:
                            dstreg.data._ptr = srcreg.data._ptr + src2reg.data._u64;
                            break;
                        case 4:
                            dstreg.data._ptr = srcreg.data._ptr + src2reg.data._u32;
                            break;
                        case 2:
                            dstreg.data._ptr = srcreg.data._ptr + src2reg.data._u16;
                            break;
                        case 1:
                            dstreg.data._ptr = srcreg.data._ptr + src2reg.data._u8;
                            break;
                        default:
                            assert(!"Wrong size for a register!");
                        }
                    } else {
                        assert(!"Wrong register type for a pointer operation!");
                    }
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
                    BINOP_UINT(dstreg, srcreg, src2reg, -);
                } else if (srcreg.type == REGTYPE_SINT) {
                    BINOP_SINT(dstreg, srcreg, src2reg, -);
                } else if (srcreg.type == REGTYPE_FLOAT) {
                    BINOP_FLOAT(dstreg, srcreg, src2reg, -);
                } else if (srcreg.type == REGTYPE_POINTER) {
                    if (src2reg.type == REGTYPE_SINT) {
                        switch (src2reg.bytes) {
                        case 8:
                            dstreg.data._ptr = srcreg.data._ptr - src2reg.data._s64;
                            break;
                        case 4:
                            dstreg.data._ptr = srcreg.data._ptr - src2reg.data._s32;
                            break;
                        case 2:
                            dstreg.data._ptr = srcreg.data._ptr - src2reg.data._s16;
                            break;
                        case 1:
                            dstreg.data._ptr = srcreg.data._ptr - src2reg.data._s8;
                            break;
                        default:
                            assert(!"Wrong size for a register!");
                        }

                    } else if (src2reg.type == REGTYPE_UINT) {
                        switch (src2reg.bytes) {
                        case 8:
                            dstreg.data._ptr = srcreg.data._ptr - src2reg.data._u64;
                            break;
                        case 4:
                            dstreg.data._ptr = srcreg.data._ptr - src2reg.data._u32;
                            break;
                        case 2:
                            dstreg.data._ptr = srcreg.data._ptr - src2reg.data._u16;
                            break;
                        case 1:
                            dstreg.data._ptr = srcreg.data._ptr - src2reg.data._u8;
                            break;
                        default:
                            assert(!"Wrong size for a register!");
                        }
                    } else {
                        assert(!"Wrong register type for a pointer operation!");
                    }
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
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));
            assert((bci->src_reg < func->num_regs) && (bci->src_reg >= 0));
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            
            switch (bci->big_const) {
            case TK_PLUS: {
                assert(bci->src_reg < func->num_regs);
                assert(bci->dst_reg < func->num_regs);
                copy_bytes(&srcreg.data._u64, &dstreg.data._u64, bci->dst_type_bytes);
                func->regs[bci->dst_reg].bytes = bci->dst_type_bytes;
                func->regs[bci->dst_reg].type = bci->dst_type;
                break;
            }
            case TK_MINUS: {
                assert(bci->src_reg < func->num_regs);
                assert(bci->dst_reg < func->num_regs);

                if (srcreg.type == REGTYPE_UINT) {
                    if (srcreg.bytes == 1) {
                        dstreg.data._s8 = -(s8)srcreg.data._u8;
                    } else if (srcreg.bytes == 2) {
                        dstreg.data._s16 = -(s16)srcreg.data._u16;
                    } else if (srcreg.bytes == 4) {
                        dstreg.data._s32 = -(s32)srcreg.data._u32;
                    } else {
                        assert(srcreg.bytes == 8);
                        dstreg.data._s64 = -(s64)srcreg.data._u64;
                    }
                    dstreg.type = REGTYPE_SINT;
                } else if (srcreg.type == REGTYPE_SINT) {
                    if (srcreg.bytes == 1) {
                        dstreg.data._s8 = -srcreg.data._s8;
                    } else if (srcreg.bytes == 2) {
                        dstreg.data._s16 = -srcreg.data._s16;
                    } else if (srcreg.bytes == 4) {
                        dstreg.data._s32 = -srcreg.data._s32;
                    } else {
                        assert(srcreg.bytes == 8);
                        dstreg.data._s64 = -srcreg.data._s64;
                    }
                    dstreg.type = REGTYPE_SINT;
                } else if (srcreg.type == REGTYPE_FLOAT) {
                    if (srcreg.bytes == 4) {
                        dstreg.data._f32 = -srcreg.data._f32;
                    } else {
                        assert(srcreg.bytes == 8);
                        dstreg.data._f64 = -srcreg.data._f64;
                    }
                    dstreg.type = REGTYPE_FLOAT;
                } else {
                    assert(!"We should never be here");
                }

                dstreg.bytes = bci->dst_type_bytes;
                break;
            }
            case TK_BANG: {
                assert(bci->src_reg < func->num_regs);
                assert(bci->dst_reg < func->num_regs);
                u64 masked_bytes = 0;

                u64 mask = 0xFFFFFFFFFFFFFFFF;
                if (srcreg.bytes < 8) {
                    mask = (1ULL << (srcreg.bytes * 8)) - 1;
                }
                masked_bytes = srcreg.data._u64 & mask;
                dstreg.data._u8 = (masked_bytes != 0) ? 0 : 1;
                dstreg.bytes = 1;
                dstreg.type = REGTYPE_UINT;
                break;
            }
            case TK_STAR: {
                assert(!"We should never be here");
                break;
            }
            case TK_LSHIFT: {
                assert(bci->src_reg < func->num_regs);
                assert(bci->dst_reg < func->num_regs);
                assert(srcreg.type == REGTYPE_POINTER);
                assert(bci->dst_type_bytes <= 8);
                switch (bci->dst_type_bytes) {
                case 1: {
                    dstreg.data._u8 = *(u8 *)srcreg.data._ptr;
                    break;
                }
                case 2: {
                    dstreg.data._u16 = *(u16 *)srcreg.data._ptr;
                    break;
                }
                case 4: {
                    dstreg.data._u32 = *(u32 *)srcreg.data._ptr;
                    break;
                }
                case 8: {
                    dstreg.data._u64 = *(u64 *)srcreg.data._ptr;
                    break;
                }
                }

                dstreg.type = bci->dst_type;
                dstreg.bytes = bci->dst_type_bytes;
                break;
            }
            default:
                assert(!"Unsupported unary operator!");
            }
            
            break;
        }
        case BC_CREATE_CALL_REGISTER: {
            // Remeber to issue a zero reg for the first reg if there is a return value!
            // (or a number of them)
            assert(bci->big_const < 256); // Sane value... 
            bc_call_register *call = new bc_call_register;
            if (bci->big_const > 0) {
                call->data = new bc_register_data[bci->big_const];
                call->type = new RegisterType[bci->big_const];
                call->bytes = new u8[bci->big_const];
            }
            call->num_regs = bci->big_const;
            assert(new_call_register == nullptr);
            s16 return_regs = bci->dst_reg;
            assert(return_regs < (s64)bci->big_const);
            s16 ind = 0;
            if (return_regs != -1) {
                ind = return_regs;
            }
            for ( ; ind < bci->big_const; ind++) {
                s16 src_reg = bci->src_reg + ind;
                assert((src_reg < func->num_regs) && (src_reg >= 0));

                copy_bytes(&func->regs[src_reg].data._u64,
                    &call->data[ind]._u64,
                    func->regs[src_reg].bytes);
                call->bytes[ind] = func->regs[src_reg].bytes;
                call->type[ind] = func->regs[src_reg].type;
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
                    // Do not support more than 1 register return
                    // Where are the type stored for the others? Too many questions for now
                    assert(bci->dst_type_bytes <= 8);
                    copy_bytes((u8 *)&current_call_register->data[0]._u64, 
                        (u8 *)&dstreg.data._u64, bci->dst_type_bytes);
                    dstreg.bytes = bci->dst_type_bytes;
                    dstreg.type = get_regtype_from_type(fundef->declaration);
                }
            }

            // We have to free up the current call register here
            // assume the return reg has already been copied

            if (current_call_register->data) {
                delete current_call_register->data;
                delete current_call_register->type;
                delete current_call_register->bytes;
            }
            delete current_call_register;

            current_call_register = old_current;
            program->bss.stack_base = old_stack_base;
            program->bss.stack_pointer = old_stack_pointer;
            break;
        }
        case BC_RETURN: {
            // end function execution right here. 
            // In the future, do postamble work such as defer
            free(func->regs);
            func->regs = old_regs;
            return; 
        }
        case BC_CAST: {
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));
            assert((bci->src_reg < func->num_regs) && (bci->src_reg >= 0));
            assert(bci->dst_type != REGTYPE_UNKNOWN);
            assert(bci->dst_type_bytes > 0);

            switch (dstreg.type) {
            case REGTYPE_FLOAT: {
                switch (srcreg.type) {
                case REGTYPE_FLOAT: {
                    f64 large = upconvertFloatReg(srcreg);
                    downconvertFloatReg(dstreg, large);
                    break;
                }
                case REGTYPE_SINT: {
                    s64 large = upconvertSIntReg(srcreg);
                    downconvertFloatReg(dstreg, (f64)large);
                    break;
                }
                case REGTYPE_UINT: {
                    u64 large = upconvertUIntReg(srcreg);
                    downconvertFloatReg(dstreg, (f64)large);
                    break;
                }
                }
                break;
            }
            case REGTYPE_SINT: {
                switch (srcreg.type) {
                case REGTYPE_FLOAT: {
                    f64 large = upconvertFloatReg(srcreg);
                    downconvertSIntReg(dstreg, (s64)large);
                    break;
                }
                case REGTYPE_SINT: {
                    s64 large = upconvertSIntReg(srcreg);
                    downconvertSIntReg(dstreg, (s64)large);
                    break;
                }
                case REGTYPE_UINT: {
                    u64 large = upconvertUIntReg(srcreg);
                    downconvertSIntReg(dstreg, (s64)large);
                    break;
                }
                }
                break;
            }
            case REGTYPE_UINT: {
                switch (srcreg.type) {
                case REGTYPE_FLOAT: {
                    f64 large = upconvertFloatReg(srcreg);
                    downconvertUIntReg(dstreg, (u64)large);
                    break;
                }
                case REGTYPE_SINT: {
                    s64 large = upconvertSIntReg(srcreg);
                    downconvertUIntReg(dstreg, (u64)large);
                    break;
                }
                case REGTYPE_UINT: {
                    u64 large = upconvertUIntReg(srcreg);
                    downconvertUIntReg(dstreg, (u64)large);
                    break;
                }
                }
                break;
            }
            default:
                assert(!"Unsupported");
                break;
            }

            dstreg.bytes = bci->dst_type_bytes;
            dstreg.type = bci->dst_type;
            break;
        }
        case BC_GOTO_CONSTANT_IF_FALSE: {
            assert((bci->src_reg < func->num_regs) && (bci->src_reg >= 0));
            assert(srcreg.type != REGTYPE_UNKNOWN);
            assert(srcreg.bytes > 0);

            u64 cond_bytes = upconvertUIntReg(srcreg);
            if (cond_bytes == 0) {
                inst_index = (u32)bci->big_const;
                assert(inst_index < func->instructions.size());
                continue;
            }
            break;
        }
        case BC_GOTO_CONSTANT_IF_TRUE: {
            assert((bci->src_reg < func->num_regs) && (bci->src_reg >= 0));
            assert(srcreg.type != REGTYPE_UNKNOWN);
            assert(srcreg.bytes > 0);

            u64 cond_bytes = upconvertUIntReg(srcreg);
            if (cond_bytes != 0) {
                inst_index = (u32)bci->big_const;
                assert(inst_index < func->instructions.size());
                continue;
            }
            break;
        }
        case BC_GOTO_CONSTANT: {
            inst_index = (u32)bci->big_const;
            assert(inst_index < func->instructions.size());
            continue;
        }
        case BC_MALLOC: {
            assert((bci->dst_reg < func->num_regs) && (bci->dst_reg >= 0));
            u64 bytes = bci->big_const;
            u8 *ptr = (u8 *)malloc(bytes);
            dstreg.data._ptr = ptr;
            dstreg.bytes = bci->dst_type_bytes;
            dstreg.type = bci->dst_type;
            break;
        }
        default:
            assert(!"Unknown Instruction type");
        }
        inst_index++;
    }
    free(func->regs);
    func->regs = old_regs;
}

void bytecode_runner::run_preamble()
{
    run_bc_function(&program->preamble_function);
}

void bytecode_generator::generate_run_directive(RunDirectiveAST *run)
{
    // TODO: we need to run the code attached to the run directive and likely create a new AST
    // or bytecode with the final result
    bytecode_function *old_current = current_function;
    RunDirectiveAST *old_run = current_run;
    auto old_ast = current_ast;
    current_ast = run;

    if (run->bc_function == nullptr) {
        bytecode_function *func = new (pool) bytecode_function;

        func->function_name = CreateTextType(pool, "Run Directive");
        func->function_id = run->s;
        run->bc_function = func;
    } else {
        // Mini optimization, reset the function here
        auto func = run->bc_function;
        func->instructions.reset();
        func->num_regs = 0;
    }
    current_function = run->bc_function;
    current_run = run;

    auto  expr_inst = computeExpression((ExpressionAST *)run->expr);
    if (!isVoidType(run->expr_type)) {
        run->reg = expr_inst->dst_reg;
        createStoreInstruction(BC_STORE_TO_CALL_REGISTER, 0, run->expr_type->size_in_bytes,
            run->reg, get_regtype_from_type(run->expr_type));
    }

    current_function = old_current;
    current_run = old_run;
    current_ast = old_ast;
    // do an early exit in case of any errors, such as those by recursive #run calls
    if (!interp->success) return;

    run->generated = true;
}

void bytecode_runner::run_directive(RunDirectiveAST * run)
{
    // Do not support returning more than 1 register
    assert(run->expr_type->size_in_bytes <= 8);

    if (run->computed) return;

    bc_register ret_val = {};
    bc_call_register ret_cr = {};
    ret_cr.data = &ret_val.data; ret_cr.num_regs = 1;
    ret_cr.bytes = &ret_val.bytes;
    ret_cr.type = &ret_val.type;
    bc_call_register * old_call = current_call_register;
    current_call_register = &ret_cr;

    // First run the bytecode in order to have the result of the run directive on the first registers
    run_bc_function(run->bc_function);
    run->computed = true;

    current_call_register = old_call;

    if (!isVoidType(run->expr_type)) {
        if (!isTypeRunSupported(run->expr_type)) {
            assert(!"Run directive with non void type return not supported yet");
        }
        LiteralAST *lit = (LiteralAST *)run->new_ast;
        assert(lit);

        // This should work for all types because it is a union underneath... 
        lit->_u64 = ret_val.data._u64;
        assert(run->bci != nullptr);
        run->bci->big_const = lit->_u64;
        if (run->enclosing_func) {
            run->enclosing_func->missing_run_directives--;
        }
    }
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

    for (s32 i = 0; i < current_call_register->num_regs; i++) {
        auto &data = current_call_register->data[i];
        auto type = current_call_register->type[i];
        auto bytes = current_call_register->bytes[i];

        // This code is so we do not push as arguments the return value regs
        if (bci->src2_reg != -1) {
            if (i < bci->src2_reg) {
                continue;
            }
        }

        if (type == REGTYPE_POINTER) {
            dcArgPointer(vm, data._ptr);
        }
        else if (type == REGTYPE_FLOAT) {
            switch (bytes) {
            case 4:
                dcArgFloat(vm, data._f32);
                break;
            case 8: 
                dcArgDouble(vm, data._f64);
                break;
            default:
                assert(false);
            }
        }
        else {
            assert((type == REGTYPE_SINT) || (type == REGTYPE_UINT));
            switch (bytes) {
            case 1:
                dcArgChar(vm, data._s8);
                break;
            case 2:
                dcArgShort(vm, data._s16);
                break;
            case 4:
                dcArgInt(vm, data._s32);
                break;
            case 8:
                dcArgLongLong(vm, data._s64);
                break;
            }
        }
    }

    dcCallVoid(vm, fundef->declaration->func_ptr);
}

