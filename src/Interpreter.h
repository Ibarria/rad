#pragma once
#include "AST.h"

enum InterpAction {
    IA_NOP,
    IA_RESOLVE_TYPE,
    IA_COMPUTE_SIZE,
    IA_OPERATION_CHECK,
};

struct interp_work {
    InterpAction action;
    BaseAST *ast;
};

struct interp_list {
    Array<interp_work *>work;
    u32 active_item = 0;

    bool empty() { return (active_item >= work.size()); }
    u64 remaining_items() 
    { 
        u64 rem = 0;
        u32 index = active_item;
        for (; index < work.size(); index++) {
            if (work[index]->action != IA_NOP) {
                rem++;
            }
        }
        return rem; 
    }
};

struct interp_deps {
    interp_list resolve_type;
    interp_list compute_size;
    interp_list operation_check;

    bool empty() { 
        return resolve_type.empty()
            && compute_size.empty()
            && operation_check.empty();
    }
    u64 remaining_items() { 
        u64 ritype  = resolve_type.remaining_items();
        u64 risize  = compute_size.remaining_items();
        u64 richeck = operation_check.remaining_items();
        return ritype + risize + richeck;
    }
};

struct Interpreter
{   
    PoolAllocator pool;
    bool success = true;
    Array<interp_deps *> overall_deps;

    Array<TextType> errors;
    char errorString[512];
    void Error(BaseAST *ast, const char *msg, ...);

    void reset_errors();

    VariableDeclarationAST *validateVariable(IdentifierAST *a);
    VariableDeclarationAST *validateFunctionCall(FunctionCallAST *a);

    bool isConstExpression(ExpressionAST *expr);
    bool compatibleTypes(TypeAST *lhs, TypeAST *rhs);

    void traversePostfixTopLevel(FileAST *root);
    void traversePostfixTopLevelDeclaration(VariableDeclarationAST *decl);
    void traversePostfixTopLevelDirective(RunDirectiveAST *run);

    void traversePostfixAST(BaseAST *ast, interp_deps &deps);

    void processAllDependencies();
    void processDependencies(interp_deps *deps); 
    u64 overallDepsItems();
    bool doWorkAST(interp_work *work);

    void perform_bytecode(FileAST *root);

    void printErrors();

    // Functions to call from outside
    void semanticProcess(FileAST *root);
};

