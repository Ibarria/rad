#include "Interpreter.h"
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include "TextType.h"
#include "bytecode_generator.h"

#ifndef WIN32
# define sprintf_s sprintf
# define vsprintf_s vsnprintf
#endif

extern bool option_printBytecode;

bool isBoolOperator(TOKEN_TYPE type);

static void printTypeToStr(char *s, TypeAST *type)
{
    switch (type->ast_type) {
    case AST_DIRECT_TYPE: {
        auto dt = (DirectTypeAST *)type;
        sprintf(s, "%s ", BasicTypeToStr(dt));
        break;
    }
    case AST_FUNCTION_TYPE: {
        auto ft = (FunctionTypeAST *)type;
        sprintf(s, "(");
        for (auto arg : ft->arguments) printTypeToStr(s, arg->specified_type);
        sprintf(s, ") ");
        if (ft->return_type) {
            printTypeToStr(s, ft->return_type);
        } else {
            sprintf(s, "-> void ");
        }
        break;
    }
    case AST_ARRAY_TYPE: {
        assert(!"Unsupported array type");
        break;
    }
    default:
        assert(!"Unsupported type");
    }
}

static void copyASTloc(BaseAST *src, BaseAST *dst)
{
    dst->filename = src->filename;
    dst->line_num = src->line_num;
}

static VariableDeclarationAST *findVariable(TextType name, Scope *scope)
{
    // trivial recursive case, could not be found
    if (scope == nullptr) return nullptr;

    for (auto d : scope->decls) {
        if (!strcmp(name, d->varname)) return d;
    }
    return findVariable(name, scope->parent);
}

static StructTypeAST *findStructType(TypeAST *type)
{
    if (type->ast_type == AST_STRUCT_TYPE) {
        return (StructTypeAST *)type;
    } else if (type->ast_type == AST_DIRECT_TYPE) {
        auto dt = (DirectTypeAST *)type;
        if (dt->basic_type != BASIC_TYPE_CUSTOM) {
            return nullptr; // normal direct types are not structs
        }
        assert(dt->custom_type);
        return findStructType(dt->custom_type);
    }
    return nullptr;
}

static bool isTypeStruct(TypeAST *type)
{
    if (type->ast_type == AST_STRUCT_TYPE) {
        return true;
    } else if (type->ast_type == AST_DIRECT_TYPE) {
        auto dt = (DirectTypeAST *)type;
        if (dt->basic_type != BASIC_TYPE_CUSTOM) {
            return false; // normal direct types are not structs
        }
        // When we reorganize the dependencies, this should go away
        if (!dt->custom_type) {
            VariableDeclarationAST *type_var = findVariable(dt->name, dt->scope);
            if (!type_var) {
                // Error(decl, "Type %s could not be resolved", dtype->name);
                assert(false);
                // we can't catch this error because we would need to be a member func
                // we need the new dependency system...
                return false;
            }
            assert(type_var->specified_type);
            dt->custom_type = type_var->specified_type;
        }
        // if we are here, we should have computed the custom type
        assert(dt->custom_type);
        // This allows for more than 1 level of indirection
        return isTypeStruct(dt->custom_type);
    }
    return false;
}

static void addTypeWork(PoolAllocator *pool, BaseAST *ast, interp_deps &deps)
{
    interp_work *res_work = new (pool) interp_work;
    res_work->action = IA_RESOLVE_TYPE;
    res_work->ast = ast;
    deps.resolve_type.work.push_back(res_work);
}

static void addSizeWork(PoolAllocator *pool, BaseAST *ast, interp_deps &deps)
{
    interp_work *res_work = new (pool) interp_work;
    res_work->action = IA_COMPUTE_SIZE;
    res_work->ast = ast;
    deps.compute_size.work.push_back(res_work);
}

static void addCheckWork(PoolAllocator *pool, BaseAST *ast, interp_deps &deps)
{
    interp_work *res_work = new (pool) interp_work;
    res_work->action = IA_OPERATION_CHECK;
    res_work->ast = ast;
    deps.operation_check.work.push_back(res_work);
}


void Interpreter::Error(BaseAST *ast, const char *msg, ...)
{
    va_list args;
    u32 off = sprintf_s(errorString, "%s:%d:%d: error : ", ast->filename, 
        ast->line_num, ast->char_num);

    va_start(args, msg);
    vsprintf_s(errorString + off, sizeof(errorString) - off, msg, args);
    va_end(args);
    success = false;

    errors.push_back(CreateTextType(&pool, errorString));
}

void Interpreter::reset_errors()
{
    success = true;
    errors.reset();
}

static FunctionDefinitionAST * findEnclosingFunction(StatementAST * stmt)
{
    Scope *scope = stmt->scope;
    do {
        if (scope->current_function) return scope->current_function;
        scope = scope->parent;
    } while (scope != nullptr);
    return nullptr;
}

VariableDeclarationAST *Interpreter::validateVariable(IdentifierAST *a)
{
    VariableDeclarationAST *decl = findVariable(a->name, a->scope);

    if (decl == nullptr) {
        if (a->prev) {
            auto prev_decl = findVariable(a->prev->name, a->prev->scope);
            assert(prev_decl->specified_type->ast_type == AST_DIRECT_TYPE);
            auto dt = (DirectTypeAST *)prev_decl->specified_type;
            Error(a, "Variable [%s] cannot be found on struct %s\n", a->name,
                dt->name);
        } else {
            Error(a, "Variable [%s] could not be found on this scope\n", a->name);
        }
        return nullptr;
    }

    if ((decl->line_num == a->line_num) && (decl->filename == a->filename)) {
        Error(a, "Variable [%s] is declared in a recursive manner\n", a->name);
    }

    if ((decl->filename == a->filename) &&
        (decl->line_num > a->line_num) &&
        !(decl->flags & DECL_FLAG_IS_CONSTANT))
    {
        Error(a, "The variable [%s] used in this declaration appears after the current declaration, this is only allowed for constants\n",
            a->name);
        return nullptr;
    }

    return decl;
}

VariableDeclarationAST *Interpreter::validateFunctionCall(FunctionCallAST *a)
{
    VariableDeclarationAST *decl = findVariable(a->function_name, a->scope);

    if (decl == nullptr) {
        Error(a, "Function [%s] could not be found on this scope\n", a->function_name);
        return nullptr;
    }

    if (decl->specified_type && decl->specified_type->ast_type != AST_FUNCTION_TYPE) {
        Error(a, "Cannot perform a function call on a variable [%s] that is not a function\n",
            a->function_name);
        return nullptr;
    }
    return decl;
}

/*
    This function returns true if both types are compatible
    One thing to be careful here is when we have arrays, that we have the array element
    type, otherwise is is a plain and simple comparison
    For pointer types, both their point_to types have to match
    Future: support type alias
*/
bool Interpreter::compatibleTypes(TypeAST * lhs, TypeAST * rhs)
{
    if (lhs->ast_type != rhs->ast_type) {
        return false;
    }
    switch (lhs->ast_type) {
    case AST_DIRECT_TYPE: {
        auto rdt = (DirectTypeAST *)rhs;
        auto ldt = (DirectTypeAST *)lhs;
        // Modify this when pointer and array support is introduced
        assert(!(ldt->isArray || ldt->isPointer || rdt->isArray || rdt->isPointer));
        return (rdt->basic_type == ldt->basic_type) && (rdt->isSigned == ldt->isSigned);
        break;
    }
    case AST_ARRAY_TYPE: {
        assert(!"Unimplemented array type");
        return false;
        break;
    }
    case AST_FUNCTION_TYPE: {
        auto rft = (FunctionTypeAST *)rhs;
        auto lft = (FunctionTypeAST *)lhs;
        if (!compatibleTypes(lft->return_type, rft->return_type)) return false;
        if (lft->arguments.size() != rft->arguments.size()) return false;
        for (u32 i = 0; i < lft->arguments.size(); i++) {
            auto larg = lft->arguments[i];
            auto rarg = rft->arguments[i];
            if (!compatibleTypes(larg->specified_type, rarg->specified_type)) return false;
        }
        return true;
        break;
    }
    default:
        return false;
    }
    return false;
}

bool Interpreter::isConstExpression(ExpressionAST *expr)
{
    switch (expr->ast_type) {
    case AST_FUNCTION_CALL: {
        return true;
    }
    case AST_IDENTIFIER: {
        IdentifierAST *a = (IdentifierAST *)expr;
        VariableDeclarationAST *decl = a->decl;

        assert(decl);

        return !!(decl->flags & DECL_FLAG_IS_CONSTANT);
    }
    case AST_LITERAL: {
        return true;
    }
    case AST_BINARY_OPERATION: {
        return true;
    }
    case AST_UNARY_OPERATION: {
        return true;
    }
    case AST_VAR_REFERENCE: {
        auto var_ref = (VarReferenceAST *)expr;
        VariableDeclarationAST *decl = findVariable(var_ref->name, var_ref->scope);
        assert(decl);

        if (decl->flags & DECL_FLAG_IS_CONSTANT) {
            return true;
        }

        var_ref = var_ref->next;

        // if the topmost element of a reference is not constant, check the rest
        while (var_ref != nullptr) {
            // At the start of the loop, we have an unchecked var_ref and the scope it applies to
            auto inner_decl = var_ref->decl;
            assert(inner_decl);

            if (inner_decl->flags & DECL_FLAG_IS_CONSTANT) {
                return true;
            }

            if (var_ref->ast_type == AST_IDENTIFIER) {
                var_ref = nullptr;
            } else if (var_ref->ast_type = AST_VAR_REFERENCE) {
                var_ref = var_ref->next;
            } else {
                assert(!"We should never be here!, wrong type on struct variable checking");
            }
        }
        // If we get here, we have not found a constant
        return false;
    }
    default:
        assert(!"We should never be here, we could not parse this type\n");
    }
    return false;
}

void Interpreter::perform_bytecode(FileAST * root)
{
    bytecode_generator bcgen;
    // @TODO : should the bytecode have its own pool? 
    // makes sense since it is emphymeral, but we need to be able to
    // get its output too...
    // Should we bytecode the whole program, or only what needs to run?
    // finding the dependencies can be tricky... 
    PoolAllocator bc_pool;   
    bcgen.setPool(&bc_pool);
    bytecode_program *bp = bcgen.compileToBytecode(root);
    
    if (option_printBytecode) print_bc_program(bp);

    // Find and run the #run directives now that we have a program compiled
    bytecode_runner runner;
    runner.program = bp;

    // Just code to test the bytecode runner
    runner.run_preamble();
    runner.run_bc_function(bp->start_function);

}

void Interpreter::printErrors()
{
    for (auto err : errors) {
        printf("%s", err);
    }
}

void Interpreter::semanticProcess(FileAST *root)
{
    traversePostfixTopLevel(root);

    processAllDependencies();

    if (!success) return;

    perform_bytecode(root);
}

void Interpreter::traversePostfixTopLevel(FileAST * root)
{
    for (auto ast : root->items) {
        switch (ast->ast_type) {
        case AST_VARIABLE_DECLARATION: {
            auto decl = (VariableDeclarationAST *)ast;
            traversePostfixTopLevelDeclaration(decl);
            break;
        }
        case AST_RUN_DIRECTIVE: {
            // @TODO: implement checks for run directive
            // At some point we want to execute the run directive, after checks
            // and sizes
            auto run = (RunDirectiveAST *)ast;
            traversePostfixTopLevelDirective(run);
            break;
        }
        default:
            assert(!"Unsupported type on top level expression");
        }

    }
}

void Interpreter::traversePostfixTopLevelDeclaration(VariableDeclarationAST * decl)
{
    assert(decl->deps == nullptr);
    decl->deps = new (&pool) interp_deps;

    traversePostfixAST(decl, *decl->deps);

    if (!decl->deps->empty()) {
        overall_deps.push_back(decl->deps);
    }
}

void Interpreter::traversePostfixTopLevelDirective(RunDirectiveAST * run)
{
    assert(!"Not implemented top level directive in new dependency system");
}

void Interpreter::traversePostfixAST(BaseAST * ast, interp_deps & deps)
{
    // null pointer is valid, like if a type does not exist 
    if (ast == nullptr) return;

    switch (ast->ast_type) {
    case AST_VARIABLE_DECLARATION: {
        auto decl = (VariableDeclarationAST *)ast;
        traversePostfixAST(decl->specified_type, deps);
        traversePostfixAST(decl->definition, deps);

        addTypeWork(&pool, ast, deps);
        addSizeWork(&pool, ast, deps);
        addCheckWork(&pool, ast, deps);
        break;
    }
    case AST_FUNCTION_TYPE: {
        auto funtype = (FunctionTypeAST *)ast;
        for (auto arg : funtype->arguments) {
            traversePostfixAST(arg, deps);
        }
        traversePostfixAST(funtype->return_type, deps);

        // No other checks for just a function type
        break;
    }
    case AST_STRUCT_DEFINITION: {
        auto struct_def = (StructDefinitionAST *)ast;
        traversePostfixAST(&struct_def->struct_type, deps);
        break;
    }
    case AST_STRUCT_TYPE: {
        auto struct_type = (StructTypeAST *)ast;
        for (auto var : struct_type->struct_scope.decls) {
            traversePostfixAST(var, deps);
        }
        addSizeWork(&pool, ast, deps);
        break;
    }
    case AST_VAR_REFERENCE: {
        auto var_ref = (VarReferenceAST *)ast;
        // Special case, where we need to process this node before
        // the next one
        addTypeWork(&pool, ast, deps);
        traversePostfixAST(var_ref->next, deps);
        // do post processing to grab the type
        addTypeWork(&pool, ast, deps);
        addSizeWork(&pool, ast, deps);
        break;
    }
    case AST_STATEMENT_BLOCK: {
        auto block = (StatementBlockAST *)ast;
        for (auto stmt : block->statements) {
            traversePostfixAST(stmt, deps);
        }
        break;
    }
    case AST_RETURN_STATEMENT: {
        auto ret = (ReturnStatementAST *)ast;
        traversePostfixAST(ret->ret, deps);

        addTypeWork(&pool, ast, deps);
        addSizeWork(&pool, ast, deps);
        addCheckWork(&pool, ast, deps);

        break;
    }
    case AST_FUNCTION_DEFINITION: {
        auto fundef = (FunctionDefinitionAST *)ast;
        traversePostfixAST(fundef->declaration, deps);
        traversePostfixAST(fundef->function_body, deps);
        // No other checks for a function definition? 

        // This check is to ensure the last statement on the block
        // matches our return type
        addCheckWork(&pool, ast, deps);

        break;
    }
    case AST_FUNCTION_CALL: {
        auto funcall = (FunctionCallAST *)ast;
        for (auto arg : funcall->args) {
            traversePostfixAST(arg, deps);
        }

        addTypeWork(&pool, ast, deps);
        addSizeWork(&pool, ast, deps);
        addCheckWork(&pool, ast, deps);

        break;
    }
    case AST_DIRECT_TYPE: {
        auto dt = (DirectTypeAST *)ast;

        addTypeWork(&pool, ast, deps);
        addSizeWork(&pool, ast, deps);

        break;
    }
    case AST_IDENTIFIER: {
        auto id = (IdentifierAST *)ast;

        addTypeWork(&pool, ast, deps);
        addSizeWork(&pool, ast, deps);
        break;
    }
    case AST_LITERAL: {
        auto lit = (LiteralAST *)ast;
        addTypeWork(&pool, ast, deps);
        break;
    }
    case AST_BINARY_OPERATION: {
        auto binop = (BinaryOperationAST *)ast;
        traversePostfixAST(binop->lhs, deps);
        traversePostfixAST(binop->rhs, deps);

        addTypeWork(&pool, ast, deps);
        addSizeWork(&pool, ast, deps);
        addCheckWork(&pool, ast, deps);
        break;
    }
    case AST_UNARY_OPERATION: {
        auto unop = (UnaryOperationAST *)ast;
        traversePostfixAST(unop->expr, deps);

        addTypeWork(&pool, ast, deps);
        addSizeWork(&pool, ast, deps);
        addCheckWork(&pool, ast, deps);
        break;
    }
    case AST_ASSIGNMENT: {
        auto assign = (AssignmentAST *)ast;
        traversePostfixAST(assign->lhs, deps);
        traversePostfixAST(assign->rhs, deps);

        addTypeWork(&pool, ast, deps);
        addSizeWork(&pool, ast, deps);
        addCheckWork(&pool, ast, deps);
        break;
    }
    default:
        assert(!"AST type not being handled on traversePostfixAST");
    }
}

void Interpreter::processAllDependencies()
{
    u64 old_remain = overallDepsItems();
    u64 current_remain = old_remain;

    do {
        for (auto dep : overall_deps) {
            processDependencies(dep);
        }
        old_remain = current_remain;
        current_remain = overallDepsItems();
        if (current_remain == 0) {
            break;
        } else if (current_remain < old_remain) {
            // if we made progress, clear up errors as they are transient
            reset_errors();
        }
    } while (current_remain < old_remain);

}

void Interpreter::processDependencies(interp_deps * deps)
{
    bool stageComplete = true;
    if (!deps->resolve_type.empty()) {

        u32 index = deps->resolve_type.active_item;
        bool firstFailure = false;
        for (; index < deps->resolve_type.work.size(); index++) {
            auto work = deps->resolve_type.work[index];
            bool r = doWorkAST(work);
            if (!r) {
                firstFailure = true;
                stageComplete = false;
                continue;
            }
            work->action = IA_NOP;
            if (!firstFailure) {
                deps->resolve_type.active_item++;
            }
        }
    }

    // One stage needs to be completely done before the next
    if (!stageComplete) return;

    if (!deps->compute_size.empty()) {

        u32 index = deps->compute_size.active_item;
        bool firstFailure = false;
        for (; index < deps->compute_size.work.size(); index++) {
            auto work = deps->compute_size.work[index];
            bool r = doWorkAST(work);
            if (!r) {
                firstFailure = true;
                stageComplete = false;
            } else {
                work->action = IA_NOP;
                if (!firstFailure) {
                    deps->compute_size.active_item++;
                }
            }
        }
    }

    // One stage needs to be completely done before the next
    if (!stageComplete) return;

    if (!deps->operation_check.empty()) {

        u32 index = deps->operation_check.active_item;
        for (; index < deps->operation_check.work.size(); index++) {
            auto work = deps->operation_check.work[index];
            bool r = doWorkAST(work);
            if (!r) {
                return;
            } else {
                work->action = IA_NOP;
                deps->operation_check.active_item++;
            }
        }
    }

}

u64 Interpreter::overallDepsItems()
{
    u64 total = 0;
    for (auto dep : overall_deps) {
        total += dep->remaining_items();
    }
    return total;
}

bool Interpreter::doWorkAST(interp_work * work)
{
    // null pointer is valid, like if a type does not exist 
    if (work->action == IA_NOP) return true;
    BaseAST *ast = work->ast;

    switch (work->ast->ast_type) {
    case AST_VARIABLE_DECLARATION: {
        auto decl = (VariableDeclarationAST *)ast;

        //traversePostfixAST(decl->specified_type, deps);
        //traversePostfixAST(decl->definition, deps);

        if (work->action == IA_RESOLVE_TYPE) {
            if (decl->specified_type) return true;

            // only when we do not have a type we better have a definition
            assert(decl->definition); // if we do not have a type we must have something to compare against
            switch (decl->definition->ast_type) {
            case AST_FUNCTION_DEFINITION: {
                FunctionDefinitionAST *fundef = (FunctionDefinitionAST *)decl->definition;
                decl->specified_type = fundef->declaration;
                decl->flags |= DECL_FLAG_HAS_BEEN_INFERRED;
                assert(decl->specified_type->size_in_bits);
                return true;
            }
            case AST_STRUCT_DEFINITION: {
                auto struct_def = (StructDefinitionAST *)decl->definition;
                decl->specified_type = &struct_def->struct_type;
                decl->flags |= DECL_FLAG_HAS_BEEN_INFERRED;
                decl->flags |= DECL_FLAG_IS_TYPE; // Use this so we do not reserve space for types
                assert(decl->specified_type->size_in_bits == 0);
                break;
            }
            default: {
                // expect this to be an expression, and the expression type needs to be deduced
                // operation, literal, function call, etc
                auto expr = (ExpressionAST *)(decl->definition);
                TypeAST *t = expr->expr_type;
                if (t != nullptr) {
                    decl->specified_type = t;
                    decl->flags |= DECL_FLAG_HAS_BEEN_INFERRED;
                    //assert(t->size_in_bits);
                    return true;
                }
                break;
            }
            }

        } else if (work->action == IA_COMPUTE_SIZE) {
            if (decl->definition && decl->definition->ast_type == AST_STRUCT_DEFINITION) {
                auto struct_def = (StructDefinitionAST *)decl->definition;
                assert((struct_def->struct_type.struct_scope.decls.size() == 0) ||
                    (struct_def->struct_type.size_in_bits > 0));
                // Go around all the members of the struct and assign relative bc_mem_offset (offset from struct parent)
                u64 bc_offset = 0;
                for (auto member : struct_def->struct_type.struct_scope.decls) {
                    member->bc_mem_offset = bc_offset;
                    bc_offset += member->specified_type->size_in_bits / 8;
                }

            }
        } else if (work->action == IA_OPERATION_CHECK) {
        } else {
            assert(!"Unknown dependency work type");
        }
        //addTypeWork(&pool, ast, deps);
        //addSizeWork(&pool, ast, deps);
        //addCheckWork(&pool, ast, deps);
        break;
    }
    case AST_FUNCTION_TYPE: {
        auto funtype = (FunctionTypeAST *)ast;
        //for (auto arg : funtype->arguments) {
        //    traversePostfixAST(arg, deps);
        //}
        //traversePostfixAST(funtype->return_type, deps);

        // No other checks for just a function type
        break;
    }
    case AST_STRUCT_DEFINITION: {
        auto struct_def = (StructDefinitionAST *)ast;
        //traversePostfixAST(&struct_def->struct_type, deps);
        break;
    }
    case AST_STRUCT_TYPE: {
        auto struct_type = (StructTypeAST *)ast;
        //for (auto var : struct_type->struct_scope.decls) {
        //    traversePostfixAST(var, deps);
        //}
        //addSizeWork(&pool, ast, deps);
        assert(work->action == IA_COMPUTE_SIZE);

        struct_type->size_in_bits = 0;
        for (auto var : struct_type->struct_scope.decls) {
            if (var->specified_type->size_in_bits == 0) {
                return false;
            }
            assert(var->specified_type->size_in_bits > 0);
            struct_type->size_in_bits += var->specified_type->size_in_bits;
        }

        break;
    }
    case AST_VAR_REFERENCE: {
        auto var_ref = (VarReferenceAST *)ast;

        if (work->action == IA_RESOLVE_TYPE) {
            if (var_ref->decl == nullptr) {
                VariableDeclarationAST *decl = findVariable(var_ref->name, var_ref->scope);
                if (decl == nullptr) {
                    if (var_ref->prev) {
                        auto prev_decl = findVariable(var_ref->prev->name, var_ref->prev->scope);
                        Error(var_ref, "Variable [%s] cannot be found on struct %s\n", var_ref->name,
                            prev_decl->varname);
                    } else {
                        Error(var_ref, "Variable [%s] could not be found on this scope\n", var_ref->name);
                    }
                    return false;
                }

                if (!isTypeStruct(decl->specified_type)) {
                    Error(var_ref, "The variable [%s] is not of struct type", var_ref->name);
                    return false;
                }

                var_ref->decl = decl;

                auto struct_type = findStructType(decl->specified_type);

                Scope *struct_scope = &struct_type->struct_scope;
                var_ref->next->scope = struct_scope;
            } else {
                if (!var_ref->next->expr_type) {
                    return false;
                }
                assert(var_ref->next->expr_type);
                var_ref->expr_type = var_ref->next->expr_type;
            }

        } else if (work->action == IA_COMPUTE_SIZE) {
            var_ref->size_in_bits = var_ref->next->size_in_bits;
        } else {
            assert(!"Not implemented");
        }

        break;
    }
    case AST_STATEMENT_BLOCK: {
        auto block = (StatementBlockAST *)ast;
        //for (auto stmt : block->statements) {
        //    traversePostfixAST(stmt, deps);
        //}
        break;
    }
    case AST_RETURN_STATEMENT: {
        auto ret_stmt = (ReturnStatementAST *)ast;
        //traversePostfixAST(ret->ret, deps);

        if (work->action == IA_RESOLVE_TYPE) {

        } else if (work->action == IA_COMPUTE_SIZE) {

        } else if (work->action == IA_OPERATION_CHECK) {
            // next we need to type check the return expression
            // with the enclosing function (!!)
            TypeAST *ret_type = ret_stmt->ret->expr_type;
            FunctionDefinitionAST *fundef = findEnclosingFunction(ret_stmt);
            if (!fundef) {
                Error(ret_stmt, "Return statement not within a function\n");
                return false;
            }
            TypeAST *func_ret_type = fundef->declaration->return_type;
            if (!compatibleTypes(ret_type, func_ret_type)) {
                char ltype[64] = {}, rtype[64] = {};
                printTypeToStr(ltype, ret_type);
                printTypeToStr(rtype, func_ret_type);
                Error(ret_stmt, "Incompatible types between the return statement: %s and the return type %s for function %s\n",
                    ltype, rtype, fundef->var_decl->varname);
            }

        } else {
            assert(!"Unknown dependency work type");
        }

        //addTypeWork(&pool, ast, deps);
        //addSizeWork(&pool, ast, deps);
        //addCheckWork(&pool, ast, deps);

        break;
    }
    case AST_FUNCTION_DEFINITION: {
        auto fundef = (FunctionDefinitionAST *)ast;
        //traversePostfixAST(fundef->declaration, deps);
        //traversePostfixAST(fundef->function_body, deps);
        // No other checks for a function definition? 

        // This check is to ensure the last statement on the block
        // matches our return type

        //addCheckWork(&pool, ast, deps);
        assert(work->action == IA_OPERATION_CHECK);


        break;
    }
    case AST_FUNCTION_CALL: {
        auto funcall = (FunctionCallAST *)ast;
        //for (auto arg : funcall->args) {
        //    traversePostfixAST(arg, deps);
        //}

        //addTypeWork(&pool, ast, deps);
        //addSizeWork(&pool, ast, deps);
        //addCheckWork(&pool, ast, deps);

        if (work->action == IA_RESOLVE_TYPE) {
            VariableDeclarationAST *decl = validateFunctionCall(funcall);
            if (!decl) return false;

            // do not recurse on inferring types as this could cause infinite recursion
            if (decl->specified_type == nullptr) {
                return false;
            }

            assert(isFunctionDeclaration(decl));
            assert(decl->definition->ast_type == AST_FUNCTION_DEFINITION);
            funcall->fundef = (FunctionDefinitionAST *)decl->definition;
            FunctionTypeAST *fundecl = (FunctionTypeAST *)decl->specified_type;
            if (!fundecl->return_type) {
                Error(funcall, "Cannot use the return value of a void function [%s]\n", funcall->function_name);
            }
            funcall->expr_type = fundecl->return_type;
            return true;

        } else if (work->action == IA_COMPUTE_SIZE) {

        } else if (work->action == IA_OPERATION_CHECK) {
            assert(funcall->fundef);

            // check that the types of expressions and function definition matches

            FunctionTypeAST *fundecl = funcall->fundef->declaration;
            if (fundecl->hasVariableArguments) {
                // for variable number of argument functions we only need to check a lower bound
                if (fundecl->arguments.size() > funcall->args.size()) {
                    Error(funcall, "Function %s called with %d arguments but it expects at least %d\n",
                        funcall->function_name, funcall->args.size(), fundecl->arguments.size());
                    return false;
                }
            } else {
                if (fundecl->arguments.size() != funcall->args.size()) {
                    Error(funcall, "Function %s called with %d arguments but it expects %d\n",
                        funcall->function_name, funcall->args.size(), fundecl->arguments.size());
                    return false;
                }
            }

            for (u32 i = 0; i < fundecl->arguments.size(); i++) {
                TypeAST *lhsType, *rhsType;
                lhsType = funcall->args[i]->expr_type;
                rhsType = fundecl->arguments[i]->specified_type;
                assert(lhsType); assert(rhsType);
                if (!compatibleTypes(lhsType, rhsType)) {
                    char ltype[64] = {}, rtype[64] = {};
                    printTypeToStr(ltype, lhsType);
                    printTypeToStr(rtype, rhsType);
                    Error(funcall, "Incompatible types during function call: provided: %s and expected: %s\n",
                        ltype, rtype);
                    return false;
                }
            }

        } else {
            assert(!"Unknown dependency work type");
        }


        break;
    }
    case AST_DIRECT_TYPE: {
        auto dt = (DirectTypeAST *)ast;

        if (work->action == IA_RESOLVE_TYPE) {
            if (dt->basic_type == BASIC_TYPE_CUSTOM) {
                // we have to find the type here and provide a reference
                if (dt->custom_type) {
                    // early out, the custom type is found
                    return true;
                } else {
                    VariableDeclarationAST *type_var = findVariable(dt->name, dt->scope);
                    if (!type_var) {
                        Error(dt, "Type %s could not be resolved", dt->name);
                        return false;
                    }
                    if (!type_var->specified_type) {
                        // this could be a declaration that happens later
                        return false;
                    }
                    assert(type_var->specified_type);
                    dt->custom_type = type_var->specified_type;
                }
            }
        } else if (work->action == IA_COMPUTE_SIZE) {
            if (dt->basic_type == BASIC_TYPE_CUSTOM) {
                assert(dt->custom_type);
                dt->size_in_bits = dt->custom_type->size_in_bits;
            }
        }
        return true;


        break;
    }
    case AST_IDENTIFIER: {
        auto id = (IdentifierAST *)ast;

        //addTypeWork(&pool, ast, deps);
        //addSizeWork(&pool, ast, deps);
        if (work->action == IA_RESOLVE_TYPE) {
            VariableDeclarationAST *decl = validateVariable(id);

            // do not recurse on inferring types as this could cause infinite recursion
            if (!decl) return false;

            id->decl = decl;
            id->expr_type = decl->specified_type;

            return true;

        } else if (work->action == IA_COMPUTE_SIZE) {
            assert(id->decl);
            assert(id->decl->specified_type);
            id->size_in_bits = id->decl->specified_type->size_in_bits;
        } else if (work->action == IA_OPERATION_CHECK) {
            assert(!"No check for a single identifier");
        } else {
            assert(!"Unknown dependency work type");
        }

        break;
    }
    case AST_LITERAL: {
        auto lit = (LiteralAST *)ast;
        // Something to do here?
        lit->expr_type = &lit->typeAST;
        break;
    }
    case AST_BINARY_OPERATION: {
        auto binop = (BinaryOperationAST *)ast;
        //traversePostfixAST(binop->lhs, deps);
        //traversePostfixAST(binop->rhs, deps);

        //addTypeWork(&pool, ast, deps);
        //addSizeWork(&pool, ast, deps);
        //addCheckWork(&pool, ast, deps);

        TypeAST *lhsType = binop->lhs->expr_type;
        TypeAST *rhsType = binop->rhs->expr_type;

        assert(lhsType);
        assert(rhsType);


        if (work->action == IA_RESOLVE_TYPE) {
            // @TODO: the type should be a combination of both lhs and rhs?

            // Very hacky! needs much more work
            if (isBoolOperator(binop->op)) {
                // the resulting operation is of type bool

                // Check for compatible types is done later
                DirectTypeAST *dt = new (&pool) DirectTypeAST;
                copyASTloc(binop, dt);
                binop->expr_type = dt;
                dt->basic_type = BASIC_TYPE_BOOL;
                dt->size_in_bits = 8;
            } else {
                // @TODO this is integer or float, the final type might need upcasting
                binop->expr_type = lhsType;
            }


        } else if (work->action == IA_COMPUTE_SIZE) {

        } else if (work->action == IA_OPERATION_CHECK) {

            // @TODO: Check that the operator matches the types of rhs, lhs
            if (!isBoolOperator(binop->op) || 
                (binop->op == TK_EQ) ||
                (binop->op == TK_NEQ)) {
                // Non boolean operators demand types that match
                // same if we check for equality
                if (!compatibleTypes(lhsType, rhsType)) {
                    char ltype[64] = {}, rtype[64] = {};
                    printTypeToStr(ltype, lhsType);
                    printTypeToStr(rtype, rhsType);
                    Error(binop, "Incompatible types during binary operation %s: %s and %s\n",
                        TokenTypeToCOP(binop->op),
                        ltype, rtype);
                    return false;
                }
            } else {
                // Here we have a comparison operator
                // @TODO: be a bit lenient on comparisons... 
                // For now the types have to match and they need to be INTEGER or FLOAT only
                if (!compatibleTypes(lhsType, rhsType)) {
                    char ltype[64] = {}, rtype[64] = {};
                    printTypeToStr(ltype, lhsType);
                    printTypeToStr(rtype, rhsType);
                    Error(binop, "Incompatible types during binary operation %s: %s and %s\n",
                        TokenTypeToCOP(binop->op),
                        ltype, rtype);
                    return false;
                }

                if ((lhsType->ast_type == rhsType->ast_type) && 
                    (lhsType->ast_type == AST_DIRECT_TYPE)) {
                    // Only support direct type (for float and integer)
                    auto dtlhs = (DirectTypeAST *)lhsType;
                    auto dtrhs = (DirectTypeAST *)rhsType;

                    if (dtlhs->basic_type != dtrhs->basic_type) {
                        char ltype[64] = {}, rtype[64] = {};
                        printTypeToStr(ltype, lhsType);
                        printTypeToStr(rtype, rhsType);
                        Error(binop, "Both types on a comparison must match: %s, %s",
                            ltype, rtype);
                        return false;
                    }
                    if ((dtlhs->basic_type != BASIC_TYPE_FLOATING) &&
                        (dtlhs->basic_type != BASIC_TYPE_INTEGER)) {
                        // we do not support string comparison 
                        char ltype[64] = {}, rtype[64] = {};
                        printTypeToStr(ltype, lhsType);
                        printTypeToStr(rtype, rhsType);
                        Error(binop, "Only Integer and Floating point numbers can be used in comparisons, but we found %s",
                            ltype);
                        return false;
                    }
                }
            }

        } else {
            assert(!"Unknown dependency work type");
        }

        break;
    }
    case AST_UNARY_OPERATION: {
        auto unop = (UnaryOperationAST *)ast;
        //traversePostfixAST(unop->expr, deps);

        //addTypeWork(&pool, ast, deps);
        //addSizeWork(&pool, ast, deps);
        //addCheckWork(&pool, ast, deps);

        if (work->action == IA_RESOLVE_TYPE) {
            TypeAST *type = unop->expr->expr_type;

            assert(type);

            // Very hacky! needs much more work
            unop->expr_type = type;

        } else if (work->action == IA_COMPUTE_SIZE) {

        } else if (work->action == IA_OPERATION_CHECK) {

        } else {
            assert(!"Unknown dependency work type");
        }

        break;
    }
    case AST_ASSIGNMENT: {
        auto assign = (AssignmentAST *)ast;
        //traversePostfixAST(assign->lhs, deps);
        //traversePostfixAST(assign->rhs, deps);

        //addTypeWork(&pool, ast, deps);
        //addSizeWork(&pool, ast, deps);
        //addCheckWork(&pool, ast, deps);

        if (work->action == IA_RESOLVE_TYPE) {
            if (assign->lhs->expr_type == nullptr) {
                return false;
            }
            assert(assign->lhs->expr_type);
            assign->expr_type = assign->lhs->expr_type;

        } else if (work->action == IA_COMPUTE_SIZE) {

        } else if (work->action == IA_OPERATION_CHECK) {

            // lhs cannot be const (or a literal, same thing)
            if (isConstExpression(assign->lhs)) {
                Error(assign, "The left hand side of an assignment must be an l-value\n");
            }

            // lhs and rhs need to have the same type
            TypeAST *lhsType, *rhsType;
            lhsType = assign->lhs->expr_type;
            rhsType = assign->rhs->expr_type;
            if (!compatibleTypes(lhsType, rhsType)) {
                char ltype[64] = {}, rtype[64] = {};
                printTypeToStr(ltype, lhsType);
                printTypeToStr(rtype, rhsType);
                Error(assign, "Incompatible types during assignment: %s and %s\n", ltype, rtype);
            }

            // @TODO: check for width of operands (in case of literals), like assigning 512 to an u8.
            // one of the hard things here is what to do on complex expressions with literals, promote?

        } else {
            assert(!"Unknown dependency work type");
        }
        break;
    }
    case AST_RUN_DIRECTIVE: {
        auto run = (RunDirectiveAST *)ast;
        if (work->action == IA_RESOLVE_TYPE) {
            TypeAST *type = run->expr->expr_type;
            assert(type);

            run->expr_type = type;
        } else {
            assert(!"Unimplemented");
        }
        break;
    }
    default:
        assert(!"AST type not being handled on traversePostfixAST");
    }

    return true;
}
