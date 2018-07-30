#pragma once
#include "AST.h"

struct bytecode_generator;
class FileData;

enum InterpAction {
    IA_NOP = 0,
    IA_RESOLVE_TYPE,
    IA_RESOLVE_TYPE_POST,
    IA_COMPUTE_SIZE,
    IA_OPERATION_CHECK,
    IA_BYTECODE,
    IA_RUN
};

struct interp_work {
    InterpAction action;
    BaseAST **ast;
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
    interp_list bytecode_generation;
    interp_list run_directives;

    bool emptySemantic() { 
        return resolve_type.empty()
            && compute_size.empty()
            && operation_check.empty();
    }
    bool emptyRun() {
        return bytecode_generation.empty()
            && run_directives.empty();
    }
    bool empty() { return emptySemantic() && emptyRun(); }
    u64 remaining_items_semantic() {
        u64 ritype  = resolve_type.remaining_items();
        u64 risize  = compute_size.remaining_items();
        u64 richeck = operation_check.remaining_items();
        return ritype + risize + richeck;
    }
    u64 remaining_items_run() {
        u64 ribyte = bytecode_generation.remaining_items();
        u64 rirun = run_directives.remaining_items();
        return ribyte + rirun;
    }
};

enum TypeCheckError {
    TCH_OK = 0,
    TCH_INCOMPATIBLE_TYPE,
    TCH_SIGN_MISMATCH,
    TCH_LITERAL_NOT_FIT,
    TCH_TYPE_NOT_FIT
};

struct Interpreter
{   
    PoolAllocator pool;
    bool success = true;
    Array<interp_deps *> overall_deps;
    Array<FileData *> files;
    bytecode_generator *bcgen = nullptr;

    char errorStringBuffer[4096];
    char *errorString = nullptr;
    void Error(BaseAST *ast, const char *msg, ...);

    void reset_errors();

    TypeCheckError checkTypesAllowLiteralAndCast(ExpressionAST **expr, TypeAST *lhs, TypeAST *rhs);

    VariableDeclarationAST *validateVariable(IdentifierAST *a);
    VariableDeclarationAST *validateFunctionCall(FunctionCallAST *a);

    bool compatibleTypes(TypeAST *lhs, TypeAST *rhs, bool &needs_cast);
    void addCast(ExpressionAST **expr, TypeAST *srcType, TypeAST *dstType);
    IdentifierAST *createIdentifier(const char *name, VariableDeclarationAST *decl);
    LiteralAST *createLiteral(ExpressionAST *expr, DirectTypeAST *type);
    VariableDeclarationAST *createDeclaration(const char *name, TypeAST *type, ExpressionAST *definition);
    VariableDeclarationAST *createDeclarationPtr(const char *name, TypeAST *type, ExpressionAST *definition);
    VariableDeclarationAST *createDeclarationSInt(const char *name, s64 start_value, BaseAST *ast);
    VariableDeclarationAST *createDeclarationUInt(const char *name, u64 start_value, BaseAST *ast);

    void traversePostfixTopLevel(FileAST *root);
    void traversePostfixTopLevelDeclaration(VariableDeclarationAST **decl);
    void traversePostfixTopLevelDirective(RunDirectiveAST **run);
    bool enforceRunDirectiveConstraints(BaseAST *ast);

    void traversePostfixAST(BaseAST **ast, interp_deps &deps);

    void processAllDependencies(FileAST *root);
    void processDependencies(interp_deps *deps);
    void processDependenciesRun(FileAST *root, interp_deps *deps);
    u64 overallDepsItemsSemantic();
    u64 overallDepsItemsRun();
    void printRemainingDependencies();
    void printRemainingDependenciesRun();
    bool doWorkAST(interp_work *work);
    bool doBytecode(interp_work *work);

    void printErrors();
    void printWork(interp_work *work, bool r);

    // Functions to call from outside
    void semanticProcess(FileAST *root);
};

