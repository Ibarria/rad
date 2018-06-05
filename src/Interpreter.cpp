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
        for (auto arg : ft->arguments) printTypeToStr(s+strlen(s), arg->specified_type);
        sprintf(s + strlen(s), ") -> ");
        printTypeToStr(s + strlen(s), ft->return_type);
        break;
    }
    case AST_POINTER_TYPE: {
        auto pt = (PointerTypeAST *)type;
        sprintf(s, "*");
        printTypeToStr(s + strlen(s), pt->points_to_type);
        break;
    }
    case AST_ARRAY_TYPE: {
        auto at = (ArrayTypeAST *)type;
        sprintf(s, "[");
        if (at->isDynamic) {
            sprintf(s + strlen(s), "..");
        } else if (at->num_elems > 0) {
            sprintf(s + strlen(s), "%d", (int)at->num_elems);
        }
        sprintf(s, "]");
        printTypeToStr(s + strlen(s), at->array_of_type);
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

bool isLValue(ExpressionAST *expr, bool allowStar)
{
    switch (expr->ast_type) {
    case AST_FUNCTION_CALL: {
        return false;
    }
    case AST_IDENTIFIER: 
    case AST_ARRAY_ACCESS:
    case AST_STRUCT_ACCESS:
        return true;
    
    case AST_LITERAL: {
        return false;
    }
    case AST_BINARY_OPERATION: {
        // All binary operations produce an rvalue
        return false;
    }
    case AST_UNARY_OPERATION: {
        auto unop = (UnaryOperationAST *)expr;
        if (unop->op == TK_LSHIFT) {
            if (allowStar) {
                return true;
            }            
            // pointer dereference always gives an lvalue
            // except two in a row are not allowed! That is checked elsewhere 
        }
        // All other unary operators return an rvalue
        return false;
    }
    default:
        assert(!"We should never be here, we could not parse this type\n");
    }
    return false;
}

/*
This function checks that an expression constant, in the sense that
the expression represents a value that can be assigned to. Similar to lvalue.
*/
bool isConstExpression(ExpressionAST *expr)
{
    if (expr == nullptr) return false;

    switch (expr->ast_type) {
    case AST_FUNCTION_CALL: {
        return true;
    }
    case AST_IDENTIFIER: {
        IdentifierAST *a = (IdentifierAST *)expr;
        VariableDeclarationAST *decl = a->decl;
        assert(decl);

        return !!(decl->flags & DECL_FLAG_IS_CONSTANT) ||
            isConstExpression(a->next);
    }
    case AST_LITERAL: {
        return true;
    }
    case AST_BINARY_OPERATION: {
        return true;
    }
    case AST_UNARY_OPERATION: {
        auto unop = (UnaryOperationAST *)expr;
        if (unop->op == TK_LSHIFT) {
            // dereferencing something does not make it constant, need to check more
            return isConstExpression(unop->expr);
        }
        // All other operands return effectively a const
        return true;
    }
    case AST_STRUCT_ACCESS: {
        auto sac = (StructAccessAST *)expr;
        VariableDeclarationAST *decl = findVariable(sac->name, sac->scope);
        assert(decl);

        return !!(decl->flags & DECL_FLAG_IS_CONSTANT) ||
            isConstExpression(sac->next);
    }
    case AST_ARRAY_ACCESS: {
        auto ac = (ArrayAccessAST *)expr;
        return isConstExpression(ac->next);
    }
    default:
        assert(!"We should never be here, we could not parse this type\n");
    }
    return false;
}

/*
  This function checks that an expression is made only of literals and 
  constant variables. This is used for array size computation. 
*/
bool isDefinedExpression(ExpressionAST *expr)
{
    switch (expr->ast_type) {
    case AST_FUNCTION_CALL: {
        // The #run directive should be used in this case
        return false;
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
        // Only when both operands are
        auto binop = (BinaryOperationAST *)expr;
        return isDefinedExpression(binop->lhs) && isDefinedExpression(binop->rhs);
    }
    case AST_UNARY_OPERATION: {
        auto unop = (UnaryOperationAST *)expr;
        // Very conservative approach
        return false;
    }
    case AST_STRUCT_ACCESS: {
        auto var_ref = (VarReferenceAST *)expr;
        // Another conservative approach. 
        // @TODO: When we have enums, this might have to change
        return false;
    }
    case AST_ARRAY_ACCESS: {
        return false;
    }
    default:
        assert(!"We should never be here, we could not parse this type\n");
    }
    return false;
}

s64 computeValue(ExpressionAST *expr)
{
    switch (expr->ast_type)
    {
    case AST_IDENTIFIER: {
        IdentifierAST *a = (IdentifierAST *)expr;
        VariableDeclarationAST *decl = a->decl;

        assert(decl);
        return computeValue((ExpressionAST *)decl->definition);
    }
    case AST_LITERAL: {
        auto lit = (LiteralAST *)expr;
        assert(lit->typeAST->basic_type == BASIC_TYPE_INTEGER);
        if (lit->typeAST->isSigned) {
            return lit->_s64;
        } else {
            return (s64)lit->_u64;
        }
    }
    case AST_BINARY_OPERATION: {
        // Only when both operands are
        auto binop = (BinaryOperationAST *)expr;
        s64 op1, op2;
        op1 = computeValue(binop->lhs);
        op2 = computeValue(binop->rhs);
        switch (binop->op)
        {
        case TK_STAR: {
            return op1 * op2;
            break;
        }
        case TK_DIV: {
            return op1 / op2;
            break;
        }
        case TK_MOD: {
            return op1 % op2;
            break;
        }
        case TK_PLUS: {
            return op1 + op2;
            break;
        }
        case TK_MINUS: {
            return op1 - op2;
            break;
        }
        default:
            assert(false);
            return 0;
        }
    }
    case AST_STRUCT_ACCESS: {
        auto var_ref = (VarReferenceAST *)expr;
        // Another conservative approach. 
        // @TODO: When we have enums, this might have to change
        return false;
    }
    case AST_ARRAY_ACCESS:
        return false;
    default:
        assert(!"We should never be here, we could not parse this type\n");
    }
    return 0;
}

StructTypeAST *findStructType(TypeAST *type)
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
    } else if (type->ast_type == AST_POINTER_TYPE) {
        // Only allow a single pointer dereference! 
        auto pt = (PointerTypeAST *)type;
        return findStructType(pt->points_to_type);
    }
    return nullptr;
}

bool isTypeStruct(TypeAST *type)
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
    } if (type->ast_type == AST_POINTER_TYPE) {
        auto pt = (PointerTypeAST *)type;
        return isTypeStruct(pt->points_to_type);
    }
    return false;
}

TypeAST *getDefinedType(VarReferenceAST *ast)
{
    switch (ast->ast_type) {
    case AST_ARRAY_ACCESS: {
        auto ac = (ArrayAccessAST *)ast;
        return ac->access_type;
        break;
    }
    case AST_STRUCT_ACCESS: {
        auto sac = (StructAccessAST *)ast;
        assert(sac->decl);
        return sac->decl->specified_type;
        break;
    }
    case AST_IDENTIFIER: {
        auto id = (IdentifierAST *)ast;
        assert(id->decl);
        return id->decl->specified_type;
        break;
    }
    default: assert(!"Invalid AST type for a reference");
        return nullptr;
    }
}

static void addTypeWork(PoolAllocator *pool, BaseAST **ast, interp_deps &deps)
{
    interp_work *res_work = new (pool) interp_work;
    res_work->action = IA_RESOLVE_TYPE;
    res_work->ast = ast;
    deps.resolve_type.work.push_back(res_work);
}

static void addTypePostWork(PoolAllocator *pool, BaseAST **ast, interp_deps &deps)
{
    interp_work *res_work = new (pool) interp_work;
    res_work->action = IA_RESOLVE_TYPE_POST;
    res_work->ast = ast;
    deps.resolve_type.work.push_back(res_work);
}

static void addSizeWork(PoolAllocator *pool, BaseAST **ast, interp_deps &deps)
{
    interp_work *res_work = new (pool) interp_work;
    res_work->action = IA_COMPUTE_SIZE;
    res_work->ast = ast;
    deps.compute_size.work.push_back(res_work);
}

static void addCheckWork(PoolAllocator *pool, BaseAST **ast, interp_deps &deps)
{
    interp_work *res_work = new (pool) interp_work;
    res_work->action = IA_OPERATION_CHECK;
    res_work->ast = ast;
    deps.operation_check.work.push_back(res_work);
}

// From Parser.cpp
DirectTypeAST *getTypeEx(DirectTypeAST *oldtype, u32 newbytes);

static void change_type_bytes(LiteralAST *lit, DirectTypeAST *dt)
{
    lit->typeAST = getTypeEx(lit->typeAST, dt->size_in_bytes);
}

static bool literal_value_fits_in_bytes(LiteralAST *lit, DirectTypeAST *dt)
{
    assert(!(lit->typeAST->isSigned && !dt->isSigned));

    if (lit->typeAST->basic_type == BASIC_TYPE_INTEGER) {
        u64 mask;
        u64 val;
        switch (dt->size_in_bytes) {
        case 1: {
            mask = 0x0FF;
            if (lit->typeAST->isSigned) {
                mask = 0x7FF;
                if (lit->_s64 < 0) val = -lit->_s64;
                else val = lit->_s64;
            } else {
                val = lit->_u64;
            }
            break;
        }
        case 2: {
            mask = 0x0FFFF;
            if (lit->typeAST->isSigned) {
                mask = 0x7FFF;
                if (lit->_s64 < 0) val = -lit->_s64;
                else val = lit->_s64;
            } else {
                val = lit->_u64;
            }
            break;
        }
        case 4: {
            mask = 0x0FFFFFFFF;
            if (lit->typeAST->isSigned) {
                mask = 0x7FFFFFFF;
                if (lit->_s64 < 0) val = -lit->_s64;
                else val = lit->_s64;
            } else {
                val = lit->_u64;
            }
            break;
        }
        case 8: {
            mask = 0xFFFFFFFFFFFFFFFFULL;
            val = lit->_u64;
            break;
        }
        default:
            assert(!"We should never be here");
            return false;
        }

        if ((val & mask) == val) {
            return true;
        }
        return false;

    } else if (lit->typeAST->basic_type == BASIC_TYPE_FLOATING) {
        // Assume floating point numbers can be cast to f32 easily

        return true; 
        // Does this even make sense? I am thinking no... 

        //f32 val = (f32)lit->_f64;
        //if (val == lit->_f64) {
        //    return true;
        //}
        //return false;
    } else {
        assert(!"We should never be here");
        return false;
    }
}

void Interpreter::Error(BaseAST *ast, const char *msg, ...)
{
    va_list args;
    u32 off = 0;
    if (ast != nullptr) {
        off = sprintf_s(errorString, "%s:%d:%d: error : ", ast->filename,
            ast->line_num, ast->char_num);
    }
    else {
        off = sprintf_s(errorString, "Compilation error : ");
    }

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

bool Interpreter::checkTypesInDeclaration(VariableDeclarationAST * decl, ExpressionAST *expr, TypeAST * lhsType, TypeAST * rhsType)
{
    if (rhsType->ast_type != lhsType->ast_type) {
        char ltype[64] = {}, rtype[64] = {};
        printTypeToStr(ltype, rhsType);
        printTypeToStr(rtype, lhsType);
        Error(decl, "Incompatible types on this variable declaration. Type: %s and the declared type %s for variable %s\n",
            ltype, rtype, decl->varname);
        return false;
    }

    if (rhsType->ast_type == AST_POINTER_TYPE) {
        // We have two type of pointers, recurse
        auto lhsPt = (PointerTypeAST *)lhsType;
        auto rhsPt = (PointerTypeAST *)rhsType;
        return checkTypesInDeclaration(decl, expr, lhsPt->points_to_type, rhsPt->points_to_type);
    }
    // Nothing else supported for now, arrays in the future
    assert(rhsType->ast_type == AST_DIRECT_TYPE);
    DirectTypeAST *rhsDType = (DirectTypeAST *)rhsType;
    DirectTypeAST *lhsDType = (DirectTypeAST *)lhsType;

    if (lhsDType->basic_type != rhsDType->basic_type) {
        if (rhsType->ast_type != lhsType->ast_type) {
            char ltype[64] = {}, rtype[64] = {};
            printTypeToStr(ltype, rhsType);
            printTypeToStr(rtype, lhsType);
            Error(decl, "Incompatible types on this variable declaration. Type: %s and the declared type %s for variable %s\n",
                ltype, rtype, decl->varname);
            return false;
        }
    }

    if (rhsDType->size_in_bytes > lhsDType->size_in_bytes) {
        if (isLiteral(expr)) {
            auto lit = (LiteralAST *)expr;

            if (!lhsDType->isSigned && rhsDType->isSigned) {
                Error(decl, "The variable %s is unsigned but a signed value is being assigned to it.\n",
                    decl->varname);
                return false;
            }

            // If the RHS is a literal, try to see if it can fit in less bytes
            if (literal_value_fits_in_bytes(lit, lhsDType)) {
                change_type_bytes(lit, lhsDType);
            } else {
                Error(decl, "Initial value for variable %s is too large for its type\n", decl->varname);
                return false;
            }
        } else {
            Error(decl, "The start value for this variable %s uses %d bytes, while the expression type can only hold %d bytes\n",
                decl->varname, rhsDType->size_in_bytes, lhsDType->size_in_bytes);
            return false;
        }
    } else {
        if (isLiteral(expr)) {
            auto lit = (LiteralAST *)expr;

            if (!lhsDType->isSigned && rhsDType->isSigned) {
                Error(decl, "The variable %s is unsigned but a signed value is being assigned to it.\n",
                    decl->varname);
                return false;
            } else if (lhsDType->isSigned && !rhsDType->isSigned &&
                (rhsDType->size_in_bytes == lhsDType->size_in_bytes)) {
                // there is a possible overflow here
                if (!literal_value_fits_in_bytes(lit, lhsDType)) {
                    Error(decl, "Initial value for variable %s is too large for its type\n", decl->varname);
                    return false;
                }
            }
        } else {
            Error(decl, "The start value for this variable %s uses %d bytes, while the expression type can only hold %d bytes\n",
                decl->varname, rhsDType->size_in_bytes, lhsDType->size_in_bytes);
            return false;
        }
    }

    return true;
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
        Error(a, "Variable [%s] could not be found on this scope\n", a->name);
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

    if (!isFunctionDeclaration(decl) && !isFunctionDefinition(decl)) {
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
        return (rdt->basic_type == ldt->basic_type) 
            && (rdt->isSigned == ldt->isSigned)
            && (rdt->size_in_bytes == ldt->size_in_bytes);
        break;
    }
    case AST_POINTER_TYPE: {
        auto rpt = (PointerTypeAST *)rhs;
        auto lpt = (PointerTypeAST *)lhs;
        return compatibleTypes(lpt->points_to_type, rpt->points_to_type);
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
    case AST_STRUCT_TYPE: {
        auto rst = (StructTypeAST *)rhs;
        auto lst = (StructTypeAST *)lhs;

        if (lst->struct_scope.decls.size() != rst->struct_scope.decls.size()) return false;
        for (u32 i = 0; i < lst->struct_scope.decls.size(); i++) {
            auto larg = lst->struct_scope.decls[i];
            auto rarg = rst->struct_scope.decls[i];
            if (!compatibleTypes(larg->specified_type, rarg->specified_type)) return false;
        }
        return true;

        break;
    }
    default:
        return false;
    }
}

static u32 overallRunItems(FileAST *root)
{
    u32 num_runs = 0;
    for (auto run : root->run_items) {
        if (run->bc_function == nullptr) num_runs++;
    }
    return num_runs;
}

static u32 overallRunDependencies(RunDirectiveAST *run)
{
    u32 num_deps = 0;
    for (auto dep : run->run_deps) {
        if (!dep->computed) num_deps++;
    }
    return num_deps;
}

static u32 totalRunsWithDeps(FileAST *root)
{
    u32 run_deps = 0;
    for (auto run : root->run_items) {
        if (overallRunDependencies(run) > 0) run_deps++;
    }
    return run_deps;
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
    bcgen.setInterpreter(this);

    CPU_SAMPLE("run bytecode");

    bytecode_program *bp = bcgen.compileToBytecode(root);

    if (!success) return;
    
    // Find and run the #run directives now that we have a program compiled
    bytecode_runner runner;
    runner.program = bp;

    assert(success);

    runner.run_preamble();
    u32 old_remain = overallRunItems(root);
    u32 current_remain = old_remain;
    do {
        for (auto run : root->run_items) {
            if (run->bc_function == nullptr) bcgen.generate_run_directive(run);
        }
        old_remain = current_remain;
        current_remain = overallRunItems(root);
        if (current_remain == 0) {
            break;
        } else if (current_remain < old_remain) {
            // if we made progress, clear up errors as they are transient
            reset_errors();
        }
    } while (current_remain < old_remain);

    if (current_remain > 0) {
        Error(nullptr, "Could not process all run directives");
        success = false;
        return;
    }

    do {
        for (auto run : root->run_items) {
            if (overallRunDependencies(run) == 0)
                runner.run_directive(run);
        }
        old_remain = current_remain;
        current_remain = totalRunsWithDeps(root);
        if (current_remain == 0) {
            break;
        } else if (current_remain < old_remain) {
            // if we made progress, clear up errors as they are transient
            reset_errors();
        }
    } while (current_remain < old_remain);

    if (current_remain > 0) {
        Error(nullptr, "Could not process all run directives");
        success = false;
        return;
    }


    // Now, all #run directives should have been processed and we can generate all the code
    bcgen.compileAllFunctions(root);
    if (!success) return;

    bcgen.validateAllFunctions();
    if (!success) return;

    if (option_printBytecode) print_bc_program(bp);
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

#define PPC(ast) (BaseAST **)(&ast)

void Interpreter::traversePostfixTopLevel(FileAST * root)
{
    CPU_SAMPLE("traversePostfix");

    for (u32 i = 0; i < root->items.size(); i++) {
        auto &ast = root->items[i];
        switch (ast->ast_type) {
        case AST_VARIABLE_DECLARATION: {
            traversePostfixTopLevelDeclaration((VariableDeclarationAST **)&root->items[i]);
            break;
        }
        case AST_RUN_DIRECTIVE: {
            // @TODO: implement checks for run directive
            // At some point we want to execute the run directive, after checks
            // and sizes
            traversePostfixTopLevelDirective((RunDirectiveAST **)&root->items[i]);
            break;
        }
        default:
            assert(!"Unsupported type on top level expression");
        }

    }
}

void Interpreter::traversePostfixTopLevelDeclaration(VariableDeclarationAST ** declp)
{
    auto decl = *declp;
    assert(decl->deps == nullptr);
    decl->deps = new (&pool) interp_deps;

    traversePostfixAST((BaseAST **)declp, *decl->deps);

    if (!decl->deps->empty()) {
        overall_deps.push_back(decl->deps);
    }
}

void Interpreter::traversePostfixTopLevelDirective(RunDirectiveAST ** runp)
{
	auto run = *runp;
    assert(run->deps == nullptr);
    run->deps = new (&pool) interp_deps;

    traversePostfixAST((BaseAST **)runp, *run->deps);

    if (!run->deps->empty()) {
        overall_deps.push_back(run->deps);
    }
}

// Returns true if the AST does not contain access to any variable or
// any non const
bool Interpreter::enforceRunDirectiveConstraints(BaseAST * ast)
{
    switch (ast->ast_type) {
    case AST_LITERAL: 
        return true;
    case AST_UNARY_OPERATION: {
        auto unop = (UnaryOperationAST *)ast;
        return enforceRunDirectiveConstraints(unop->expr);
    }
    case AST_BINARY_OPERATION: {
        auto binop = (BinaryOperationAST *)ast;
        bool left, right;
        left = enforceRunDirectiveConstraints(binop->lhs);
        right = enforceRunDirectiveConstraints(binop->rhs);
        return left && right;
    }
    case AST_FUNCTION_CALL: {
        auto funcall = (FunctionCallAST *)ast;
        bool valid = true;
        for (auto arg : funcall->args) {
            valid = valid && enforceRunDirectiveConstraints(arg);
            if (!valid) return false;
        }
        return valid;
    }
    case AST_IDENTIFIER: {
        auto id = (IdentifierAST *)ast;
        VariableDeclarationAST *decl = validateVariable(id);

        // do not recurse on inferring types as this could cause infinite recursion
        if (!decl) return false;
        if (!(decl->flags & DECL_FLAG_IS_CONSTANT)) {
            Error(ast, "Run directive can only use constant or literals, variable %s is neither.", id->name);
            return false;
        }
        return true;
    }
    default:
        Error(ast, "Run directive is only suported on function calls, unary and binary operations that use constants or literals");
        return false;
    }
    return true;
}

void Interpreter::traversePostfixAST(BaseAST ** astp, interp_deps & deps)
{
    auto ast = *astp;
    // null pointer is valid, like if a type does not exist 
    if (ast == nullptr) return;

    switch (ast->ast_type) {
    case AST_VARIABLE_DECLARATION: {
        auto decl = (VariableDeclarationAST *)ast;
        traversePostfixAST( PPC(decl->specified_type), deps);
        traversePostfixAST( PPC(decl->definition), deps);

        addTypeWork(&pool, astp, deps);
        addSizeWork(&pool, astp, deps);
        addCheckWork(&pool, astp, deps);
        break;
    }
    case AST_FUNCTION_TYPE: {
        auto funtype = (FunctionTypeAST *)ast;
        for (u32 i = 0; i < funtype->arguments.size(); i++) {
            traversePostfixAST(PPC(funtype->arguments[i]), deps);
        }
        traversePostfixAST(PPC(funtype->return_type), deps);

        // No other checks for just a function type
        break;
    }
    case AST_STRUCT_DEFINITION: {
        auto struct_def = (StructDefinitionAST *)ast;
        traversePostfixAST(PPC(struct_def->struct_type), deps);
        break;
    }
    case AST_STRUCT_TYPE: {
        auto struct_type = (StructTypeAST *)ast;
        for (u32 i = 0; i < struct_type->struct_scope.decls.size(); i++) {
            traversePostfixAST(PPC(struct_type->struct_scope.decls[i]), deps);
        }
        addSizeWork(&pool, astp, deps);
        break;
    }
    case AST_STRUCT_ACCESS: {
        auto sac = (StructAccessAST *)ast;
        // Special case, where we need to process this node before
        // the next one
        addTypeWork(&pool, astp, deps);
        traversePostfixAST(PPC(sac->next), deps);
        // do post processing to grab the type
        addTypePostWork(&pool, astp, deps);
        addSizeWork(&pool, astp, deps);
        break;
    }
    case AST_ARRAY_ACCESS: {
        auto acc = (ArrayAccessAST *)ast;
        // Special case, where we need to process this node before
        // the next one
        traversePostfixAST(PPC(acc->array_exp), deps);
        addTypeWork(&pool, astp, deps);
        if (acc->next != nullptr) {
            traversePostfixAST(PPC(acc->next), deps);
        }
        // do post processing to grab the type
        addTypePostWork(&pool, astp, deps);
        addSizeWork(&pool, astp, deps);
        break;
    }
    case AST_STATEMENT_BLOCK: {
        auto block = (StatementBlockAST *)ast;
        for (u32 i = 0; i < block->statements.size(); i++) {
            traversePostfixAST(PPC(block->statements[i]), deps);
        }
        break;
    }
    case AST_RETURN_STATEMENT: {
        auto ret = (ReturnStatementAST *)ast;
        traversePostfixAST(PPC(ret->ret), deps);

        addTypeWork(&pool, astp, deps);
        addSizeWork(&pool, astp, deps);
        addCheckWork(&pool, astp, deps);

        break;
    }
    case AST_IF_STATEMENT: {
        auto ifst = (IfStatementAST *)ast;
        traversePostfixAST(PPC(ifst->condition), deps);
        traversePostfixAST(PPC(ifst->then_branch), deps);
        traversePostfixAST(PPC(ifst->else_branch), deps);

        addTypeWork(&pool, astp, deps);
        addSizeWork(&pool, astp, deps);
        addCheckWork(&pool, astp, deps);
        break;
    }
    case AST_FUNCTION_DEFINITION: {
        auto fundef = (FunctionDefinitionAST *)ast;
        traversePostfixAST(PPC(fundef->declaration), deps);
        traversePostfixAST(PPC(fundef->function_body), deps);
        // No other checks for a function definition? 

        // This check is to ensure the last statement on the block
        // matches our return type
        addCheckWork(&pool, astp, deps);

        break;
    }
    case AST_FUNCTION_CALL: {
        auto funcall = (FunctionCallAST *)ast;
        for (u32 i = 0; i < funcall->args.size(); i++) {
            traversePostfixAST(PPC(funcall->args[i]), deps);
        }

        addTypeWork(&pool, astp, deps);
        addSizeWork(&pool, astp, deps);
        addCheckWork(&pool, astp, deps);

        break;
    }
    case AST_DIRECT_TYPE: {
        auto dt = (DirectTypeAST *)ast;

        addTypeWork(&pool, astp, deps);
        addSizeWork(&pool, astp, deps);

        break;
    }
    case AST_POINTER_TYPE: {
        auto pt = (PointerTypeAST *)ast;
        traversePostfixAST(PPC(pt->points_to_type), deps);

        // No work triggered by the pointer type itself so far
        break;
    }
    case AST_ARRAY_TYPE: {
        auto at = (ArrayTypeAST *)ast;
        if (at->num_expr != nullptr) traversePostfixAST(PPC(at->num_expr), deps);
        traversePostfixAST(PPC(at->array_of_type), deps);

        addTypeWork(&pool, astp, deps);
        addSizeWork(&pool, astp, deps);
        break;
    }
    case AST_IDENTIFIER: {
        auto id = (IdentifierAST *)ast;

        addTypeWork(&pool, astp, deps);
        traversePostfixAST(PPC(id->next), deps);
        addTypePostWork(&pool, astp, deps);
        addSizeWork(&pool, astp, deps);
        break;
    }
    case AST_LITERAL: {
        auto lit = (LiteralAST *)ast;
        addTypeWork(&pool, astp, deps);
        break;
    }
    case AST_BINARY_OPERATION: {
        auto binop = (BinaryOperationAST *)ast;
        traversePostfixAST(PPC(binop->lhs), deps);
        traversePostfixAST(PPC(binop->rhs), deps);

        addTypeWork(&pool, astp, deps);
        addSizeWork(&pool, astp, deps);
        addCheckWork(&pool, astp, deps);
        break;
    }
    case AST_UNARY_OPERATION: {
        auto unop = (UnaryOperationAST *)ast;
        traversePostfixAST(PPC(unop->expr), deps);

        addTypeWork(&pool, astp, deps);
        //addSizeWork(&pool, ast, deps);
        //addCheckWork(&pool, ast, deps);
        break;
    }
    case AST_ASSIGNMENT: {
        auto assign = (AssignmentAST *)ast;
        traversePostfixAST(PPC(assign->lhs), deps);
        traversePostfixAST(PPC(assign->rhs), deps);

        addTypeWork(&pool, astp, deps);
        addSizeWork(&pool, astp, deps);
        addCheckWork(&pool, astp, deps);
        break;
    }
	case AST_RUN_DIRECTIVE: {
		auto run = (RunDirectiveAST *)ast;
		traversePostfixAST(PPC(run->expr), deps);

        addTypeWork(&pool, astp, deps);
//        addSizeWork(&pool, astp, deps);
        addCheckWork(&pool, astp, deps);

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

    CPU_SAMPLE("processAllDependencies");

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

    if (current_remain > 0) {
        Error(nullptr, "Could not process all dependencies");
        success = false;
    }
}

void Interpreter::processDependencies(interp_deps * deps)
{
    CPU_SAMPLE("processDependencies");

    bool stageComplete = true;
    if (!deps->resolve_type.empty()) {
        CPU_SAMPLE("TypeDependencies");

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
        CPU_SAMPLE("SizeDependencies");

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
        CPU_SAMPLE("CheckDependencies");

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
    BaseAST *ast = *(work->ast);

    switch (ast->ast_type) {
    case AST_VARIABLE_DECLARATION: {
        auto decl = (VariableDeclarationAST *)ast;

        //traversePostfixAST(decl->specified_type, deps);
        //traversePostfixAST(decl->definition, deps);

        if (work->action == IA_RESOLVE_TYPE) {
            if (decl->specified_type) {
                return true;
            }

            // only when we do not have a type we better have a definition
            assert(decl->definition); // if we do not have a type we must have something to compare against
            switch (decl->definition->ast_type) {
            case AST_FUNCTION_DEFINITION: {
                FunctionDefinitionAST *fundef = (FunctionDefinitionAST *)decl->definition;
                decl->specified_type = fundef->declaration;
                decl->flags |= DECL_FLAG_HAS_BEEN_INFERRED;
                assert(decl->specified_type->size_in_bytes);
                return true;
            }
            case AST_STRUCT_DEFINITION: {
                auto struct_def = (StructDefinitionAST *)decl->definition;
                decl->specified_type = struct_def->struct_type;
                decl->flags |= DECL_FLAG_HAS_BEEN_INFERRED;
                decl->flags |= DECL_FLAG_IS_TYPE; // Use this so we do not reserve space for types
                assert(decl->specified_type->size_in_bytes == 0);
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
                else {
                    return false;
                }
                break;
            }
            }

        } else if (work->action == IA_COMPUTE_SIZE) {
            if (decl->definition && decl->definition->ast_type == AST_STRUCT_DEFINITION) {
                auto struct_def = (StructDefinitionAST *)decl->definition;
                assert((struct_def->struct_type->struct_scope.decls.size() == 0) ||
                    (struct_def->struct_type->size_in_bytes > 0));
                // Go around all the members of the struct and assign relative bc_mem_offset (offset from struct parent)
                u64 bc_offset = 0;
                for (auto member : struct_def->struct_type->struct_scope.decls) {
                    member->bc_offset = bc_offset;
                    bc_offset += member->specified_type->size_in_bytes;
                }

            }
        } else if (work->action == IA_OPERATION_CHECK) {
            // Check here for the size of operands on the type itself and if it has a definition
            if (decl->definition) { // Nothing to do if there is no definition

                if (decl->flags & DECL_FLAG_HAS_BEEN_INFERRED) {
                    // nothing to do, the types match for sure when inferred
                    return true;
                }

                switch (decl->definition->ast_type) {
                case AST_FUNCTION_DEFINITION: {
                    FunctionDefinitionAST *fundef = (FunctionDefinitionAST *)decl->definition;
                    TypeAST *dtype = fundef->declaration;
                    TypeAST *stype = decl->specified_type; 
                    
                    if (!compatibleTypes(stype, stype)) {
                        char ltype[64] = {}, rtype[64] = {};
                        printTypeToStr(ltype, stype);
                        printTypeToStr(rtype, dtype);
                        Error(decl, "Incompatible types on this variable declaration. Type: %s and the declared type %s for variable %s\n",
                            ltype, rtype, decl->varname);
                        return false;
                    }
                    return true;
                }
                case AST_STRUCT_DEFINITION: {
                    auto struct_def = (StructDefinitionAST *)decl->definition;
                    TypeAST *dtype = struct_def->struct_type;
                    TypeAST *stype = decl->specified_type;
                    if (!compatibleTypes(stype, stype)) {
                        char ltype[64] = {}, rtype[64] = {};
                        printTypeToStr(ltype, stype);
                        printTypeToStr(rtype, dtype);
                        Error(decl, "Incompatible types on this variable declaration. Type: %s and the declared type %s for variable %s\n",
                            ltype, rtype, decl->varname);
                        return false;
                    }
                    return true;
                }
                default: {
                    auto expr = (ExpressionAST *)(decl->definition);
                    TypeAST *rhsType = expr->expr_type;
                    TypeAST *lhsType = decl->specified_type;
                    assert(rhsType);

                    return checkTypesInDeclaration(decl, expr, lhsType, rhsType);
                }
                }
            }
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

        struct_type->size_in_bytes = 0;
        for (auto var : struct_type->struct_scope.decls) {
            if (var->specified_type->size_in_bytes == 0) {
                return false;
            }
            assert(var->specified_type->size_in_bytes > 0);
            struct_type->size_in_bytes += var->specified_type->size_in_bytes;
        }

        break;
    }
    case AST_STRUCT_ACCESS: {
        auto sac = (StructAccessAST *)ast;

        if (work->action == IA_RESOLVE_TYPE) {
            if (sac->decl == nullptr) {
                assert(sac->prev);
                TypeAST *enclosingType = getDefinedType(sac->prev);
                if (!isTypeStruct(enclosingType)) {
                    char ltype[64] = {};
                    printTypeToStr(ltype, enclosingType);
                    Error(sac, "Struct access with member %s cannot be done on this type: %s\n",
                        sac->name, ltype);
                    return false;
                }

                // Configure the scope from the prev once validated
                StructTypeAST *stype = findStructType(enclosingType);
                Scope *struct_scope = &stype->struct_scope;
                sac->scope = struct_scope;


                VariableDeclarationAST *decl = findVariable(sac->name, sac->scope);
                if (decl == nullptr) {
                    if (stype->decl == nullptr) {
                        // This is likely a declaration yet to come, stop here
                        return false;
                    }
                    auto prev_decl = stype->decl;
                    Error(sac, "Variable [%s] cannot be found on struct %s\n", sac->name,
                        prev_decl->varname);
                    return false;
                }

                sac->decl = decl;

            }
        } else if (work->action == IA_RESOLVE_TYPE_POST) {
            // This can indicate an earlier failure, no more work to do here
            if (sac->decl == nullptr) return false;

            if (sac->next) {
                if (!sac->next->expr_type) {
                    return false;
                }
                sac->expr_type = sac->next->expr_type;
            } else {
                sac->expr_type = sac->decl->specified_type;
            }

        } else if (work->action == IA_COMPUTE_SIZE) {
            if (sac->next) {
                sac->size_in_bytes = sac->next->size_in_bytes;
            } else {
                sac->size_in_bytes = sac->decl->specified_type->size_in_bytes;
            }
        } else {
            assert(!"Not implemented");
        }

        break;
    }
    case AST_ARRAY_ACCESS: {
        auto acc = (ArrayAccessAST *)ast;

        if (work->action == IA_RESOLVE_TYPE) {
            assert(acc->prev);
            if (!acc->access_type) {

                TypeAST *enclosingType = getDefinedType(acc->prev);

                if (enclosingType->ast_type != AST_ARRAY_TYPE) {
                    char ltype[64] = {};
                    printTypeToStr(ltype, enclosingType);
                    Error(acc, "Only array types can use the [ ] operator, found: %s\n", ltype);
                    return false;
                }

                assert(acc->array_exp);
                TypeAST *atype = acc->array_exp->expr_type;
                if (atype->ast_type != AST_DIRECT_TYPE) {
                    char ltype[64] = {};
                    printTypeToStr(ltype, atype);
                    Error(acc, "Only integer values can be used to index an array, found: %s\n", ltype);
                    return false;
                }
                auto dt = (DirectTypeAST *)acc->array_exp->expr_type;
                if (dt->basic_type != BASIC_TYPE_INTEGER) {
                    char ltype[64] = {};
                    printTypeToStr(ltype, atype);
                    Error(acc, "Only integer values can be used to index an array, found: %s\n", ltype);
                    return false;
                }
                auto artype = (ArrayTypeAST *)enclosingType;

                acc->access_type = artype->array_of_type;

            }
        } else if (work->action == IA_RESOLVE_TYPE_POST) {
            // This can indicate an earlier failure, no more work to do here
            if (acc->access_type == nullptr) return false;

            if (acc->next != nullptr) {
                if (!acc->next->expr_type) {
                    assert(!"How could we ever get here???");
                    return false;
                }
                assert(acc->next->expr_type);
                acc->expr_type = acc->next->expr_type;
            } else {
                acc->expr_type = acc->access_type;
            }

        } else if (work->action == IA_COMPUTE_SIZE) {
            if (acc->next) {
                acc->size_in_bytes = acc->next->size_in_bytes;
            } else {
                acc->size_in_bytes = acc->access_type->size_in_bytes;
            }
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
    case AST_IF_STATEMENT: {
        auto ifst = (IfStatementAST *)ast;

        if (work->action == IA_RESOLVE_TYPE) {

        }
        else if (work->action == IA_COMPUTE_SIZE) {

        }
        else if (work->action == IA_OPERATION_CHECK) {
            // next we need to type check the if statement
            TypeAST *cond_type = ifst->condition->expr_type;

            if (cond_type == nullptr) return false;

            if (!isTypeBoolean(cond_type) && !isTypePointer(cond_type) && 
                                             !isTypeInteger(cond_type)) {
                Error(ifst, "The condition expression has to evaluate to an Integer, Boolean or pointer\n");
                return false;
            }
        } else {
            assert(!"Unknown dependency work type");
        }

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
            if ((decl->specified_type == nullptr) && !isFunctionDefinition(decl)) {
                // For constant functions, of type f :: () -> int {return 1;} , 
                // have no specified type but have a definition
                return false;
            }

            assert(isFunctionDefinition(decl));
            assert(decl->definition->ast_type == AST_FUNCTION_DEFINITION);
            funcall->fundef = (FunctionDefinitionAST *)decl->definition;

            if (decl->specified_type == nullptr) {
                decl->specified_type = funcall->fundef->declaration;
                decl->flags |= DECL_FLAG_HAS_BEEN_INFERRED;
            }

            FunctionTypeAST *fundecl = (FunctionTypeAST *)decl->specified_type;
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
                dt->size_in_bytes = dt->custom_type->size_in_bytes;
            }
        }
        return true;


        break;
    }
    case AST_POINTER_TYPE: {
        auto pt = (PointerTypeAST *)ast;

        // No actions for now... 
        return true;
        break;
    }
    case AST_ARRAY_TYPE: {
        auto at = (ArrayTypeAST *)ast;
        //if (at->num_expr != nullptr) traversePostfixAST(PPC(at->num_expr), deps);
        //traversePostfixAST(PPC(at->array_of_type), deps);

        //addTypeWork(&pool, astp, deps);
        //addSizeWork(&pool, astp, deps);

        if (work->action == IA_RESOLVE_TYPE) {
            
        } else if (work->action == IA_COMPUTE_SIZE) {
            if (at->num_expr) {
                if (!isDefinedExpression(at->num_expr)) {
                    Error(at, "Array needs to have a constant and defined size for number of elements\n");
                    return false;
                }
                if (at->num_expr->expr_type->ast_type != AST_DIRECT_TYPE) {
                    Error(at, "Array needs to have an integer type for their size\n");
                    return false;
                }
                auto dt = (DirectTypeAST *)at->num_expr->expr_type;
                if (dt->basic_type != BASIC_TYPE_INTEGER) {
                    Error(at, "Array needs to have an integer type for their size\n");
                    return false;
                }
                at->num_elems = computeValue(at->num_expr);
                at->size_in_bytes = (u32)at->num_elems * at->array_of_type->size_in_bytes;
            } else {
                if (at->isDynamic) {
                    // Data pointer, Total num elems, Used Elems
                    at->size_in_bytes = 8 + 8 + 8; 
                } else {
                    // Data pointer, Total num elems
                    at->size_in_bytes = 8 + 8;
                }
            }
        }
        return true;
        break;
    }
    case AST_IDENTIFIER: {
        auto id = (IdentifierAST *)ast;

        if (work->action == IA_RESOLVE_TYPE) {
            if (id->decl == nullptr) {
                VariableDeclarationAST *decl = validateVariable(id);

                // do not recurse on inferring types as this could cause infinite recursion
                if (!decl) return false;

                id->decl = decl;
            }
        } else if (work->action == IA_RESOLVE_TYPE_POST) {
            // This can indicate an earlier failure, no more work to do here
            if (id->decl == nullptr) return false;

            if (id->next) {
                id->expr_type = id->next->expr_type;
            } else {
                // if we do not have a type for the declaration yet, do not consider this done
                if (id->decl->specified_type == nullptr) return false;

                id->expr_type = id->decl->specified_type;
            }

            return true;

        } else if (work->action == IA_COMPUTE_SIZE) {
            assert(id->decl);
            if (isFunctionDefinition(id->decl)) {
                // Function definitions do not have a size, as they do not get reserved space
                // just pass
            } else {
                assert(id->decl->specified_type);
                id->size_in_bytes = id->expr_type->size_in_bytes;
            }
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
        lit->expr_type = lit->typeAST;
        break;
    }
    case AST_BINARY_OPERATION: {
        auto binop = (BinaryOperationAST *)ast;
        //traversePostfixAST(binop->lhs, deps);
        //traversePostfixAST(binop->rhs, deps);

        //addTypeWork(&pool, ast, deps);
        //addSizeWork(&pool, ast, deps);
        //addCheckWork(&pool, ast, deps);


        /*
            Binary operations can only have direct types
            or descendents (pointer, or a direct type)
            Array is expected to be indexed, though. 
        */

        TypeAST *lhsType = (DirectTypeAST *)binop->lhs->expr_type;
        TypeAST *rhsType = (DirectTypeAST *)binop->rhs->expr_type;

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
                dt->size_in_bytes = 1;
            } else {
                // We assume that both types are the same
                // Or that we have upcasting automatically happen
                if (lhsType->ast_type == AST_DIRECT_TYPE) {
                    auto lhsDtType = (DirectTypeAST *)lhsType;
                    DirectTypeAST *dt = new (&pool) DirectTypeAST;
                    copyASTloc(binop, dt);
                    binop->expr_type = dt;
                    dt->basic_type = lhsDtType->basic_type;
                    dt->size_in_bytes = lhsDtType->size_in_bytes;
                    // The optimization for Literal will require replacement
                } else if (lhsType->ast_type == AST_POINTER_TYPE) {
                    PointerTypeAST *pt = new (&pool) PointerTypeAST;
                    auto lhsPtType = (PointerTypeAST *)lhsType;

                    copyASTloc(binop, pt);
                    binop->expr_type = pt;
                    pt->points_to_type = lhsPtType->points_to_type;
                    pt->size_in_bytes = 8;
                }
            }

        } else if (work->action == IA_COMPUTE_SIZE) {

        } else if (work->action == IA_OPERATION_CHECK) {

            if (lhsType->ast_type == AST_POINTER_TYPE) {
                /* Pointers are a special case
                   We allow The following operations:
                   PTR == PTR (all the boolean operators)
                   PTR + / - UINT/SINT  Only add or substract ints
                */

                if (isBoolOperator(binop->op)) {
                    if (!compatibleTypes(lhsType, rhsType)) {
                        char ltype[64] = {}, rtype[64] = {};
                        printTypeToStr(ltype, lhsType);
                        printTypeToStr(rtype, rhsType);
                        Error(binop, "Incompatible types during binary operation %s: %s and %s\n",
                            TokenTypeToCOP(binop->op),
                            ltype, rtype);
                        return false;
                    }
                    return true;
                }

                if ((binop->op == TK_PLUS) || (binop->op == TK_MINUS)) {
                    if (rhsType->ast_type != AST_DIRECT_TYPE) {
                        char ltype[64] = {}, rtype[64] = {};
                        printTypeToStr(ltype, lhsType);
                        printTypeToStr(rtype, rhsType);
                        Error(binop, "Incompatible type during pointer arithmetic operation: %s\n",
                            rtype);
                        return false;
                    }
                    auto rhsDt = (DirectTypeAST *)rhsType;
                    if (rhsDt->basic_type != BASIC_TYPE_INTEGER) {
                        char ltype[64] = {}, rtype[64] = {};
                        printTypeToStr(ltype, lhsType);
                        printTypeToStr(rtype, rhsType);
                        Error(binop, "Incompatible type during pointer arithmetic: %s\n",
                            rtype);
                        return false;
                    }
                    return true;
                }

                // All other operations are not allowed with pointers
                // like *, /, MOD, shift...
                {
                    char ltype[64] = {}, rtype[64] = {};
                    printTypeToStr(ltype, lhsType);
                    printTypeToStr(rtype, rhsType);
                    Error(binop, "Incompatible types during pointer binary operation %s: %s and %s\n",
                        TokenTypeToCOP(binop->op),
                        ltype, rtype);
                    return false;
                }
            }

            // we better be a direct type if we get to this point
            assert(lhsType->ast_type == AST_DIRECT_TYPE);

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
                // @TODO: be a bit lenient on comparisons..., with auto cast
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

            if ((unop->op == TK_PLUS) || (unop->op == TK_MINUS)) {
                // We only support number types
                if (type->ast_type != AST_DIRECT_TYPE) {
                    char ltype[64] = {};
                    printTypeToStr(ltype, type);
                    Error(unop, "Unary operation [%s] is only allowed on numeric types, found: %s\n",
                        TokenTypeToCOP(unop->op), ltype);
                    return false;
                }

                auto dt = (DirectTypeAST *)type;
                if (!((dt->basic_type == BASIC_TYPE_FLOATING) || (dt->basic_type == BASIC_TYPE_INTEGER))) {
                    char ltype[64] = {};
                    printTypeToStr(ltype, type);
                    Error(unop, "Unary operation [%s] is only allowed on numeric types, found: %s\n",
                        TokenTypeToCOP(unop->op), ltype);
                    return false;
                }
                // take the type and bytes from the enclosing expression
                if ((unop->op == TK_PLUS) || dt->isSigned) {
                    unop->expr_type = type;
                } else {
                    // if we use the - operand, and before we had an unsigned value, now we have
                    // a signed one
                    DirectTypeAST *dtype = new (&pool) DirectTypeAST;
                    copyASTloc(unop, dtype);
                    dtype->basic_type = dt->basic_type;
                    dtype->isSigned = true;
                    dtype->size_in_bytes = dt->size_in_bytes;
                    unop->expr_type = dtype;
                }
                return true;
            } else if (unop->op == TK_BANG) {
                // bang negates booleans, and for numbers and pointers, converts non 0 to true
                switch (type->ast_type) {
                case AST_STRUCT_TYPE:
                case AST_FUNCTION_TYPE:
                case AST_ARRAY_TYPE: {
                    char ltype[64] = {};
                    printTypeToStr(ltype, type);
                    Error(unop, "Unary operation [%s] is only allowed on numeric or pointer or boolean types, found: %s\n",
                        TokenTypeToCOP(unop->op), ltype);
                    return false;
                }
                case AST_DIRECT_TYPE: {
                    auto dt = (DirectTypeAST *)type;

                    if ((dt->basic_type != BASIC_TYPE_FLOATING) && (dt->basic_type != BASIC_TYPE_INTEGER) &&
                        (dt->basic_type != BASIC_TYPE_BOOL)) {
                        char ltype[64] = {};
                        printTypeToStr(ltype, type);
                        Error(unop, "Unary operation [%s] is only allowed on numeric or boolean types, found: %s\n",
                            TokenTypeToCOP(unop->op), ltype);
                        return false;
                    }                
                    // exit the switch for success
                    break;
                }
                case AST_POINTER_TYPE: {
                    // trivial success here, exit the switch
                    break;
                }
                }

                DirectTypeAST *dt = new (&pool) DirectTypeAST;
                copyASTloc(unop, dt);
                dt->basic_type = BASIC_TYPE_BOOL;
                dt->size_in_bytes = 1;
                unop->expr_type = dt;
                return true;
            } else if (unop->op == TK_STAR) {
                // The address operator works only on lvalues

                if (!isLValue(unop->expr, false)) {
                    Error(unop, "It is not possible to use the address operator [*] on an rvalue\n");
                }

                if (unop->expr->ast_type == AST_UNARY_OPERATION) {
                    auto innerop = (UnaryOperationAST *)unop->expr;
                    if (innerop->op == TK_LSHIFT) {
                        // Simplify the * << val to just be val
                        *work->ast = innerop->expr;
                        return true;
                    }
                }

                PointerTypeAST *pt = new (&pool) PointerTypeAST;
                copyASTloc(unop, pt);
                pt->points_to_type = type;
                pt->size_in_bytes = 8;
                unop->expr_type = pt;
                return true;

            } else if (unop->op == TK_LSHIFT) {
                // Pointer dereference only works on Pointer types
                if (type->ast_type != AST_POINTER_TYPE) {
                    char ltype[64] = {};
                    printTypeToStr(ltype, type);
                    Error(unop, "Pointer dereference operator [%s] is only allowed on pointer types, found: %s\n",
                        TokenTypeToCOP(unop->op), ltype);
                    return false;
                }
                auto pt = (PointerTypeAST *)type;
                unop->expr_type = pt->points_to_type;
                return true;
            } else {
                assert(!"Unknown operand type");
                return false;
            }

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
            if (!isLValue(assign->lhs)) {
                Error(assign, "The left hand side of an assignment must be an l-value\n");
            }

            if (isConstExpression(assign->lhs)) {
                Error(assign, "The left hand side of an assignment cannot be constant\n");
            }

            // lhs and rhs need to have the same type
            // @TODO: support auto upcast
            TypeAST *lhsType, *rhsType;
            lhsType = assign->lhs->expr_type;
            rhsType = assign->rhs->expr_type;

            if (rhsType->ast_type != lhsType->ast_type) {
                char ltype[64] = {}, rtype[64] = {};
                printTypeToStr(ltype, rhsType);
                printTypeToStr(rtype, lhsType);
                Error(assign, "Incompatible types on this assignment. Type: %s and the declared type %s\n",
                    ltype, rtype);
                return false;
            }

            if (lhsType->ast_type == AST_DIRECT_TYPE) {
                assert(rhsType->ast_type == AST_DIRECT_TYPE);
                DirectTypeAST *rhsDType = (DirectTypeAST *)rhsType;
                DirectTypeAST *lhsDType = (DirectTypeAST *)lhsType;

                if (lhsDType->basic_type != rhsDType->basic_type) {
                    char ltype[64] = {}, rtype[64] = {};
                    printTypeToStr(ltype, lhsType);
                    printTypeToStr(rtype, rhsType);
                    Error(assign, "Incompatible types on this assignment: %s and %s\n",
                        ltype, rtype);
                    return false;
                }

                if (!isLiteral(assign->rhs)) {
                    // If it is not a literal, we can only do very harsh decisions
                    if (rhsDType->size_in_bytes > lhsDType->size_in_bytes) {
                        char ltype[64] = {}, rtype[64] = {};
                        printTypeToStr(ltype, lhsType);
                        printTypeToStr(rtype, rhsType);
                        Error(assign, "Assignment right hand side type [%s] is too large for the left hand side [%s]\n",
                            ltype, rtype);
                        return false;
                    }
                    if ((rhsDType->size_in_bytes == lhsDType->size_in_bytes) && (lhsDType->isSigned != rhsDType->isSigned)) {
                        char ltype[64] = {}, rtype[64] = {};
                        printTypeToStr(ltype, lhsType);
                        printTypeToStr(rtype, rhsType);
                        Error(assign, "The assignment having mismatched sign types: %s and %s.\n", ltype, rtype);
                        return false;
                    }
                    if ((rhsDType->size_in_bytes < lhsDType->size_in_bytes) && (!lhsDType->isSigned && rhsDType->isSigned)) {
                        char ltype[64] = {}, rtype[64] = {};
                        printTypeToStr(ltype, lhsType);
                        printTypeToStr(rtype, rhsType);
                        Error(assign, "The assignment trying to assign a signed type %s to an unsigned one %s.\n", rtype, ltype);
                        return false;
                    }
                } else {
                    auto lit = (LiteralAST *)assign->rhs;
                    if (rhsDType->size_in_bytes > lhsDType->size_in_bytes) {

                        if (!lhsDType->isSigned && rhsDType->isSigned) {
                            char ltype[64] = {}, rtype[64] = {};
                            printTypeToStr(ltype, lhsType);
                            printTypeToStr(rtype, rhsType);
                            Error(assign, "The assignment having mismatched sign types: %s and %s.\n", ltype, rtype);
                            return false;
                        }

                        // If the RHS is a literal, try to see if it can fit in less bytes
                        if (literal_value_fits_in_bytes(lit, lhsDType)) {
                            change_type_bytes(lit, lhsDType);
                        } else {
                            char ltype[64] = {}, rtype[64] = {};
                            printTypeToStr(ltype, lhsType);
                            printTypeToStr(rtype, rhsType);
                            Error(assign, "Assignment right hand side type [%s] is too large for the left hand side [%s]\n",
                                ltype, rtype);
                            return false;
                        }
                    } else {

                        if (!lhsDType->isSigned && rhsDType->isSigned) {
                            Error(assign, "The assignment is trying to assign a signed value to an unsigned one.\n");
                            return false;
                        } else if (((lhsDType->isSigned == rhsDType->isSigned) ||
                                    (lhsDType->isSigned))
                                             &&
                            (rhsDType->size_in_bytes == lhsDType->size_in_bytes)) {
                            // there is a possible overflow here
                            if (!literal_value_fits_in_bytes(lit, lhsDType)) {
                                char ltype[64] = {}, rtype[64] = {};
                                printTypeToStr(ltype, lhsType);
                                printTypeToStr(rtype, rhsType);
                                Error(assign, "Assignment right hand side type [%s] is too large for the left hand side [%s]\n",
                                    ltype, rtype);
                                return false;
                            }
                        } else {
                            char ltype[64] = {}, rtype[64] = {};
                            printTypeToStr(ltype, lhsType);
                            printTypeToStr(rtype, rhsType);
                            Error(assign, "Assignment right hand side type [%s] is too large for the left hand side [%s]\n",
                                ltype, rtype);
                            return false;
                        }
                    }

                }
                return true;

            } else {
                // for non direct types, just check the basic compatible types checks

                if (!compatibleTypes(lhsType, rhsType)) {
                    char ltype[64] = {}, rtype[64] = {};
                    printTypeToStr(ltype, lhsType);
                    printTypeToStr(rtype, rhsType);
                    Error(assign, "Incompatible types during assignment: %s and %s\n", ltype, rtype);
                }
                return true;
            }

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
        } else if (work->action == IA_OPERATION_CHECK) {
            // Run directives can only depend on constants/literals
            auto res = enforceRunDirectiveConstraints(run->expr);
            return res;
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
