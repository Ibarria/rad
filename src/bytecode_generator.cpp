#include "bytecode_generator.h"

const u64 stack_size = 10 * 1024;

static u64 getScopeVariablesSize(Scope *scope)
{
    u64 total_size = 0;
    for (auto decl : scope->decls) {
        assert(decl->specified_type);
        assert(decl->specified_type->size_in_bits > 0);
        assert((decl->specified_type->size_in_bits % 4) == 0);
        total_size += decl->specified_type->size_in_bits / 4;
    }
    return total_size;
}

static inline u64 roundToPage(u64 size, u64 page_size)
{
    size = (page_size - 1)&size ? ((size + page_size) & ~(page_size - 1)) : size;
    return size;
}

bytecode_program * bytecode_generator::compileToBytecode(FileAST * root)
{
    bytecode_program *bp = new (pool) bytecode_program;
    this->program = bp;

    u64 bss_size = getScopeVariablesSize(&root->scope);
    // ensure we get page aligned memory chunks
    bss_size = roundToPage(bss_size, 4 * 1024);

    bp->bss.initMem((u8 *)pool->alloc(bss_size + stack_size),
        bss_size, stack_size);

    // First step, have space in the bss for the global variables (non functions)
    
    // set the correct value in the bss area for vars (bytecode instruction)

    // Next, write functions in the bss. If calling a function, we might (or might not)
    // have its address, keep it in mind and later update all locations

    return bp;
}

void bc_base_memory::initMem(u8 * basemem, u64 bss_size, u64 stack_size)
{
    mem = basemem;
    alloc_size = bss_size + stack_size;
    used_size = 0;
    stack_base = mem + bss_size;
    stack_index = 0;
    this->stack_size = stack_size;
}
