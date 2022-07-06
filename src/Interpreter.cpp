#include "Interpreter.h"
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include "TextType.h"
#include "bytecode_generator.h"
#include "FileData.h"

#ifndef WIN32
# define sprintf_s sprintf
# define vsprintf_s vsnprintf
#endif

// This helps see how dependencies are actually being processed
#define DEBUG_DEPS 0

extern bool option_printBytecode;
extern u64 sequence_id;

bool isBoolOperator(TOKEN_TYPE type);
DirectTypeAST *getBuiltInType(TOKEN_TYPE tktype);

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
        if (isDynamicArray(at)) {
            sprintf(s + strlen(s), "..");
        } else if (isStaticArray(at)) {
            sprintf(s + strlen(s), "%u", (u32)at->num_elems);
        }
        sprintf(s + strlen(s), "] ");
        printTypeToStr(s + strlen(s), at->array_of_type);
        break;
    }
    case AST_NULL_TYPE: {
        sprintf(s, "nullptr ");
        break;
    }
    default:
        assert(!"Unsupported type");
    }
}

static void copyASTloc(BaseAST *src, BaseAST *dst)
{
    dst->filename = src->filename;
    dst->char_num = src->char_num;
    dst->line_num = src->line_num;
}

static VariableDeclarationAST *findVariable(TextType name, Scope *scope)
{
    // trivial recursive case, could not be found
    if (scope == nullptr) return nullptr;

    for (auto d : scope->decls) {
        if (name == d->varname) return d;
    }
    return findVariable(name, scope->parent);
}

static VariableDeclarationAST *findVariable(TextType name, Array<VariableDeclarationAST *>& decls)
{
    for (auto d : decls) {
        if (name == d->varname) return d;
    }
    return nullptr;
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
    case AST_NULL_PTR: {
        return false;
    }
    default:
        assert(!"We should never be here, we could not parse this type\n");
    }
    return false;
}

/*
This function checks that an expression constant, in the sense that
the expression represents a value that cannot be assigned to. Similar to lvalue.
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
    case AST_NULL_PTR: {
        return true;
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

s64 computeValueInt(ExpressionAST *expr)
{
    switch (expr->ast_type)
    {
    case AST_IDENTIFIER: {
        IdentifierAST *a = (IdentifierAST *)expr;
        VariableDeclarationAST *decl = a->decl;

        assert(decl);
        assert(isConstantDeclaration(decl));
        assert(decl->definition);
        return computeValueInt((ExpressionAST *)decl->definition);
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
        op1 = computeValueInt(binop->lhs);
        op2 = computeValueInt(binop->rhs);
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
        assert(!"We should never be here, call isDefinedExpression first");
        return false;
    }
    case AST_ARRAY_ACCESS:
        assert(!"We should never be here, call isDefinedExpression first");
        return false;
    default:
        assert(!"We should never be here, we could not parse this type\n");
    }
    return 0;
}

f64 computeValueFloat(ExpressionAST *expr)
{
    switch (expr->ast_type)
    {
    case AST_IDENTIFIER: {
        IdentifierAST *a = (IdentifierAST *)expr;
        VariableDeclarationAST *decl = a->decl;

        assert(decl);
        assert(isConstantDeclaration(decl));
        assert(decl->definition);
        return computeValueFloat((ExpressionAST *)decl->definition);
    }
    case AST_LITERAL: {
        auto lit = (LiteralAST *)expr;
        assert(lit->typeAST->basic_type == BASIC_TYPE_FLOATING);
        return lit->_f64;
    }
    case AST_BINARY_OPERATION: {
        // Only when both operands are
        auto binop = (BinaryOperationAST *)expr;
        f64 op1, op2;
        op1 = computeValueFloat(binop->lhs);
        op2 = computeValueFloat(binop->rhs);
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
            assert(!"Illegal operation");
            return 0;
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
        assert(!"We should never be here, call isDefinedExpression first");
        return 0;
    }
    case AST_ARRAY_ACCESS:
        assert(!"We should never be here, call isDefinedExpression first");
        return 0;
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

bool isTypeStructOrPtr(TypeAST *type)
{
    if (type->ast_type == AST_POINTER_TYPE) {
        auto pt = (PointerTypeAST *)type;
        return isTypeStructOrPtr(pt->points_to_type);
    }
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
            if (!type_var->specified_type) {
                // @Cleanup: ensure this false triggers a re-evaluation later of types
                return false;
            }
            assert(type_var->specified_type);
            dt->custom_type = type_var->specified_type;
        }
        // if we are here, we should have computed the custom type
        assert(dt->custom_type);
        // This allows for more than 1 level of indirection
        return isTypeStructOrPtr(dt->custom_type);
    }
    return false;
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

static void addBytecodeWork(PoolAllocator *pool, BaseAST **ast, interp_deps &deps)
{
    interp_work *res_work = new (pool) interp_work;
    res_work->action = IA_BYTECODE;
    res_work->ast = ast;
    deps.bytecode_generation.work.push_back(res_work);
}

static void addRunWork(PoolAllocator *pool, BaseAST **ast, interp_deps &deps)
{
    interp_work *res_work = new (pool) interp_work;
    res_work->action = IA_RUN;
    res_work->ast = ast;
    deps.run_directives.work.push_back(res_work);
}

// From Parser.cpp
DirectTypeAST *getTypeEx(DirectTypeAST *oldtype, u32 newbytes);

static void change_type_bytes(LiteralAST *lit, DirectTypeAST *dt)
{
    lit->typeAST = getTypeEx(lit->typeAST, dt->size_in_bytes);
}

static bool value_fits_in_bytes(s64 val, DirectTypeAST *dt)
{
    u64 mask;

    // This is done to remove the sign and allow mask to work
    if (val < 0) val = -val;

    switch (dt->size_in_bytes) {
    case 1: {
        mask = 0x0FF;
        break;
    }
    case 2: {
        mask = 0x0FFFF;
        break;
    }
    case 4: {
        mask = 0x0FFFFFFFF;
        break;
    }
    case 8: {
        mask = 0xFFFFFFFFFFFFFFFFULL;
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
}

static bool value_fits_in_bytes(f64 val, DirectTypeAST *dt)
{
    switch (dt->size_in_bytes) {
    case 4: {
        f32 val2 = (f32)val;
        return val2 == val;
    }
    case 8: {
        return true;
    }
    default:
        assert(!"We should never be here");
        return false;
    }

    return false;
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

static bool findAst(BaseAST *root, BaseAST *elem)
{
    if ((root == nullptr) || (elem == nullptr)) return false;
    if (root == elem) return true;
    
    switch (root->ast_type) {
    case AST_IDENTIFIER: {
        auto id = (IdentifierAST *)root;
        return findAst(id->next, elem);
        break;
    }
    case AST_LITERAL: {
        return false;
        break;
    }
    case AST_BINARY_OPERATION: {
        auto binop = (BinaryOperationAST *)root;
        return (findAst(binop->lhs, elem) || findAst(binop->rhs, elem));
        break;
    }
    case AST_UNARY_OPERATION: {
        auto unop = (UnaryOperationAST *)root;
        return findAst(unop->expr, elem);
        break;
    }
    case AST_CAST: {
        auto cast = (CastAST *)root;
        return findAst(cast->expr, elem);
        break;
    }
    case AST_FUNCTION_CALL: {
        auto funcall = (FunctionCallAST *)root;
        for (auto arg : funcall->args) {
            if (findAst(arg, elem)) {
                return true;
            }
        }
        return false;
        break;
    }
    case AST_ARRAY_ACCESS: {
        auto acc = (ArrayAccessAST *)root;
        if (findAst(acc->array_exp, elem)) return true;
        return findAst(acc->next, elem);
        break;
    }
    case AST_STRUCT_ACCESS: {
        auto sac = (StructAccessAST *)root;
        return findAst(sac->next, elem);
        break;
    }
    default:
        // This means we need to support more AST types
        assert(false);
        return false;
    }
}

void Interpreter::Error(BaseAST *ast, const char *msg, ...)
{
    va_list args;
    u32 off = 0;
    FileData *fd = nullptr;

    if (!errorString) {
        errorString = errorStringBuffer;
        *errorString = 0;
    }

    if (ast != nullptr) {
        off = sprintf(errorString, "%s:%d:%d: error : ", ast->filename,
            ast->line_num, ast->char_num);
        for (auto f : files) {
            if (!strcmp(f->getFilename(), ast->filename)) {
                fd = f;
                break;
            }
        }
    }
    else {
        off = sprintf(errorString, "Compilation error : ");
    }

    va_start(args, msg);
    off += vsprintf(errorString + off, msg, args);
    va_end(args);
    success = false;

    errorString += off;
    if (fd) {
        SrcLocation loc;
        loc.line = ast->line_num; loc.col = ast->char_num;
        errorString = fd->printLocation(loc, errorString);
    }
}

void Interpreter::reset_errors()
{
    success = true;
    errorString = errorStringBuffer;
    *errorString = 0;
}

TypeCheckError Interpreter::checkTypesAllowLiteralAndCast(ExpressionAST **expr, TypeAST * lhsType, TypeAST * rhsType)
{
    char ltype[64] = {}, rtype[64] = {};
    printTypeToStr(ltype, rhsType);
    printTypeToStr(rtype, lhsType);
    bool needs_cast = false;
    bool ret;

    if (rhsType->ast_type != lhsType->ast_type) {
        return TCH_INCOMPATIBLE_TYPE;
    }

    if (rhsType->ast_type == AST_POINTER_TYPE) {
        // Just use the code in compatibleTypes
        ret = compatibleTypes(lhsType, rhsType, needs_cast);
        if (!ret) {
            return TCH_INCOMPATIBLE_TYPE;
        }
    } else if (rhsType->ast_type == AST_ARRAY_TYPE) {
        bool ret = compatibleTypes(lhsType, rhsType, needs_cast);
        if (!ret) {
            return TCH_INCOMPATIBLE_TYPE;
        }
    } else if (rhsType->ast_type == AST_DIRECT_TYPE) {
        DirectTypeAST *rhsDType = (DirectTypeAST *)rhsType;
        DirectTypeAST *lhsDType = (DirectTypeAST *)lhsType;

        if (lhsDType->basic_type != rhsDType->basic_type) {
            // by default, if the basic types do not match, we will just say no 
            // (demand explicit cast)
            // There is one exception to this rule, Integer (for defined expressions)
            // converts to float (not the other way around)            
            if ((lhsDType->basic_type == BASIC_TYPE_FLOATING) &&
                (rhsDType->basic_type == BASIC_TYPE_INTEGER) &&
                isDefinedExpression(*expr)) {
                s64 ival = computeValueInt(*expr);
                LiteralAST *lit = createLiteral(*expr, lhsDType);
                lit->_f64 = (f64)ival;
                *expr = lit;
                return TCH_OK;
            }

            return TCH_INCOMPATIBLE_TYPE;
        }

        if (rhsDType->size_in_bytes > lhsDType->size_in_bytes) {
            if (isDefinedExpression(*expr)) {
                if (!lhsDType->isSigned && rhsDType->isSigned) {
                    return TCH_SIGN_MISMATCH;
                }

                if (isLiteral(*expr)) {
                    auto lit = (LiteralAST *)*expr;

                    // If the RHS is a literal, try to see if it can fit in less bytes
                    if (literal_value_fits_in_bytes(lit, lhsDType)) {
                        change_type_bytes(lit, lhsDType);
                        return TCH_OK;
                    } else {
                        return TCH_LITERAL_NOT_FIT;
                    }
                } else {
                    // we have an expression that is more than a literal. 
                    // We need to know the type first:
                    if (rhsDType->basic_type == BASIC_TYPE_INTEGER) {
                        s64 val = computeValueInt(*expr);
                        if (value_fits_in_bytes(val, lhsDType)) {
                            LiteralAST *lit = createLiteral(*expr, lhsDType);
                            lit->_s64 = val;
                            *expr = lit;
                            return TCH_OK;
                        } else {
                            return TCH_LITERAL_NOT_FIT;
                        }
                    } else if (rhsDType->basic_type == BASIC_TYPE_FLOATING) {
                        f64 val = computeValueFloat(*expr);
                        if (value_fits_in_bytes(val, lhsDType)) {
                            LiteralAST *lit = createLiteral(*expr, lhsDType);
                            lit->_f64 = val;
                            *expr = lit;
                            return TCH_OK;
                        } else {
                            return TCH_LITERAL_NOT_FIT;
                        }
                    } else {
                        assert(!"we should never be here");
                    }
                }
            } else {
                return TCH_TYPE_NOT_FIT;
            }
            if (isLiteral(*expr)) {
                auto lit = (LiteralAST *)*expr;

                if (!lhsDType->isSigned && rhsDType->isSigned) {
                    return TCH_SIGN_MISMATCH;
                }

                // If the RHS is a literal, try to see if it can fit in less bytes
                if (literal_value_fits_in_bytes(lit, lhsDType)) {
                    change_type_bytes(lit, lhsDType);
                } else {
                    return TCH_LITERAL_NOT_FIT;
                }
            } else {
                return TCH_TYPE_NOT_FIT;
            }
        } else {
            if (!lhsDType->isSigned && rhsDType->isSigned) {
                return TCH_SIGN_MISMATCH;
            }

            if (isLiteral(*expr)) {
                auto lit = (LiteralAST *)*expr;

                if (lhsDType->isSigned && !rhsDType->isSigned &&
                    (rhsDType->size_in_bytes == lhsDType->size_in_bytes)) {
                    // there is a possible overflow here
                    if (!literal_value_fits_in_bytes(lit, lhsDType)) {
                        return TCH_LITERAL_NOT_FIT;
                    }
                }
            } else {

                // I am not sure about this one... 
                if (rhsDType->size_in_bytes == lhsDType->size_in_bytes) {
                    return TCH_OK;
                }

                // Should we automatically upconvert?
                return TCH_TYPE_NOT_FIT;
            }
        }

        return TCH_OK;
    } else if (rhsType->ast_type == AST_FUNCTION_TYPE) {
        bool ret = compatibleTypes(lhsType, rhsType, needs_cast);
        if (!ret || needs_cast) {
            return TCH_INCOMPATIBLE_TYPE;
        }
        
    } else if (rhsType->ast_type == AST_STRUCT_TYPE) {
        bool ret = compatibleTypes(lhsType, rhsType, needs_cast);
        if (!ret || needs_cast) {
            return TCH_INCOMPATIBLE_TYPE;
        }
    } else {
        assert(!"Unsupported conversion type");
    }

    if (needs_cast) addCast(expr, rhsType, lhsType);
    return TCH_OK;
}

FunctionDefinitionAST * findEnclosingFunction(BaseAST * ast)
{
    Scope *scope = ast->scope;
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

    // This error is to prevent cases where a variable refers to itself on declaration:
    // it: int = it;
    // The check has to be careful, comparing only line numbers can find issue with:
    // it : int; it = 3; (for example)
    if ((decl->line_num == a->line_num) && (decl->filename == a->filename)) {
        if (decl->definition != nullptr) {
            // in this case, do a recursive search
            if (findAst(decl->definition, a)) {
                Error(a, "Variable [%s] is declared in a recursive manner\n", a->name);
                return nullptr;
            }
        }
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
bool Interpreter::compatibleTypes(TypeAST * lhs, TypeAST * rhs, bool &needs_cast)
{
    needs_cast = false;
    // special case for null pointers
    if (isTypePointer(lhs) && isTypeNullPtr(rhs)) return true;

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
        bool ret = compatibleTypes(lpt->points_to_type, rpt->points_to_type, needs_cast);
        // Do not allow casting of pointer types
        return (ret && !needs_cast);
        break;
    }
    case AST_ARRAY_TYPE: {
        auto rt = (ArrayTypeAST *)rhs;
        auto lt = (ArrayTypeAST *)lhs;
        // Compatible array rules
        /*
            - Support Static and Dynamic conversion to Sized (variable assignment)
            - Arrays must be of the same type and dimensionality (recursion)
            - Array comparison is not supported! @TODO: verify
        
        
        */
        // first, the types of the arrays have to be the same
        if (!compatibleTypes(lt->array_of_type, rt->array_of_type, needs_cast)) {
            return false;
        }
        if (needs_cast) {
            // We do not allow casting of the element inside the array
            return false;
        }
        if (lt->array_type == rt->array_type) {
            switch (lt->array_type) {
            case ArrayTypeAST::STATIC_ARRAY: {
                assert(lt->num_elems != 0);
                return lt->num_elems == rt->num_elems;
            }
            case ArrayTypeAST::SIZED_ARRAY: {
                return true;
            }
            case ArrayTypeAST::DYNAMIC_ARRAY: {
                return true;
            }
            default:
                assert(!"We should never be here, unknown array type");
            }
            
        } else if (lt->array_type == ArrayTypeAST::SIZED_ARRAY) {
            // Any array can be converted to a sized array, as long as the inner types match
            needs_cast = true;
            return true;
        }
            
        return false;
        break;
    }
    case AST_FUNCTION_TYPE: {
        auto rft = (FunctionTypeAST *)rhs;
        auto lft = (FunctionTypeAST *)lhs;
        if (!compatibleTypes(lft->return_type, rft->return_type, needs_cast)) return false;
        if (needs_cast) return false;
        if (lft->arguments.size() != rft->arguments.size()) return false;
        for (u32 i = 0; i < lft->arguments.size(); i++) {
            auto larg = lft->arguments[i];
            auto rarg = rft->arguments[i];
            if (!compatibleTypes(larg->specified_type, rarg->specified_type, needs_cast)) return false;
            if (needs_cast) return false;
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
            if (!compatibleTypes(larg->specified_type, rarg->specified_type, needs_cast)) return false;
            if (needs_cast) return false;
        }
        return true;

        break;
    }
    default:
        return false;
    }
}

void Interpreter::addCast(ExpressionAST ** expr, TypeAST * srcType, TypeAST * dstType)
{
    CastAST *cast = new (&pool) CastAST();
    copyASTloc(*expr, cast);
    cast->s = sequence_id++;
    cast->isImplicit = true;
    cast->expr = *expr;
    cast->expr_type = dstType;
    cast->srcType = srcType;
    cast->dstType = dstType;
    *expr = cast;
}

IdentifierAST * Interpreter::createIdentifier(const char * name, VariableDeclarationAST * decl)
{
    IdentifierAST *id = new (&pool) IdentifierAST();
    copyASTloc(decl, id);
    id->s = sequence_id++;
    id->name = CreateTextType(&pool, name);
    id->decl = decl;
    id->size_in_bytes = decl->specified_type->size_in_bytes;
    id->expr_type = decl->specified_type;
    id->scope = decl->scope;
    return id;
}

LiteralAST * Interpreter::createLiteral(ExpressionAST * expr, DirectTypeAST * type)
{
    LiteralAST *lit = new (&pool) LiteralAST();
    copyASTloc(expr, lit);
    lit->s = sequence_id++;
    lit->expr_type = type;
    lit->typeAST = type;
    return lit;
}

VariableDeclarationAST * Interpreter::createDeclaration(const char * name, TypeAST * type, ExpressionAST * definition)
{
    VariableDeclarationAST *decl = new (&pool) VariableDeclarationAST();
    if (definition != nullptr) copyASTloc(definition, decl);
    decl->s = sequence_id++;
    decl->varname = CreateTextType(&pool, name);
    decl->definition = definition;
    decl->specified_type = type;
    return decl;
}

VariableDeclarationAST * Interpreter::createDeclarationPtr(const char * name, TypeAST * type, ExpressionAST * definition)
{
    PointerTypeAST *ptype = new (&pool) PointerTypeAST();
    copyASTloc(type, ptype);
    ptype->s = sequence_id++;
    ptype->points_to_type = type;
    ptype->size_in_bytes = 8;
    return createDeclaration(name, ptype, definition);
}


VariableDeclarationAST * Interpreter::createDeclarationSInt(const char * name, s64 start_value, BaseAST * ast)
{
    DirectTypeAST *type = getBuiltInType(TK_S64);
    LiteralAST *lit = new (&pool) LiteralAST();
    copyASTloc(ast, lit);
    lit->expr_type = type;
    lit->s = sequence_id++;
    lit->typeAST = type;
    lit->_s64 = start_value;
    // lit->scope (is this needed?)
    return createDeclaration(name, type, lit);
}

VariableDeclarationAST * Interpreter::createDeclarationUInt(const char * name, u64 start_value, BaseAST * ast)
{
    DirectTypeAST *type = getBuiltInType(TK_U64);
    LiteralAST *lit = new (&pool) LiteralAST();
    copyASTloc(ast, lit);
    lit->expr_type = type;
    lit->s = sequence_id++;
    lit->typeAST = type;
    lit->_u64 = start_value;
    // lit->scope (is this needed?)
    return createDeclaration(name, type, lit);
}

static u32 overallRunDependencies(RunDirectiveAST *run)
{
    u32 num_deps = 0;
    for (auto dep : run->run_deps) {
        if (!dep->computed) num_deps++;
    }
    return num_deps;
}

void Interpreter::printErrors()
{
    printf("%s", errorStringBuffer);
}

static const char *WorkToStr[] = {
    "NOP",
    "TYPE",
    "TPOST",
    "SIZE",
    "CHECK",
    "BCODE",
    "RUN"
};

void Interpreter::printWork(interp_work * work, bool r)
{
    printf("    >> %6s %4s %s:%d,%d : %s\n", WorkToStr[work->action], 
        (r ? "OK": "FAIL"),
        (*work->ast)->filename, (*work->ast)->line_num,
        (*work->ast)->char_num, AstClassTypeToStr((*work->ast)->ast_type));
}

void Interpreter::semanticProcess(FileAST *root)
{
    traversePostfixTopLevel(root);

    processAllDependencies(root);

    if (!success) return;
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

        // only global declarations need bytecode work, the rest
        // are handled when we bytecode a function
        if (isGlobalDeclaration(decl)) addBytecodeWork(&pool, astp, deps);
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
        addCheckWork(&pool, astp, deps);
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
        addCheckWork(&pool, astp, deps);
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

        // Only do dependency processing for custom types
        if (dt->basic_type == BASIC_TYPE_CUSTOM) {
            addTypeWork(&pool, astp, deps);
            addSizeWork(&pool, astp, deps);
        }

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
        addCheckWork(&pool, astp, deps);
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
    case AST_NEW: {
        auto nast = (NewAllocAST *)ast;
        traversePostfixAST(PPC(nast->type), deps);

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

        addBytecodeWork(&pool, astp, deps);
        addRunWork(&pool, astp, deps);
		break;
	}
    case AST_FOR_STATEMENT: {
        auto forst = (ForStatementAST *)ast;
        if (forst->is_array) {
            traversePostfixAST(PPC(forst->arr), deps);
        } else {
            traversePostfixAST(PPC(forst->start), deps);
            traversePostfixAST(PPC(forst->end), deps);
        }

        addTypeWork(&pool, astp, deps);
        //        addSizeWork(&pool, astp, deps);
        addCheckWork(&pool, astp, deps);

        // We do this here so the iterators can find the declaration
        // and get all the work setup
        traversePostfixAST(PPC(forst->it), deps);
        traversePostfixAST(PPC(forst->it_index), deps);

        // traverse the block latest as we should know all on the loop
        traversePostfixAST(PPC(forst->loop_block), deps);
        break;
    }
    case AST_NULL_PTR: {
        auto nptr = (NullPtrAST *)ast;
        // Nothing to do here, we created the type with null on the parser
        break;
    }
    default:
        assert(!"AST type not being handled on traversePostfixAST");
    }
}

void Interpreter::processAllDependencies(FileAST *root)
{
    u64 old_remain = overallDepsItemsSemantic();
    u64 current_remain = old_remain;

    CPU_SAMPLE("processAllDependencies");

    bytecode_generator bcgen;
    // @TODO : should the bytecode have its own pool? 
    // makes sense since it is emphymeral, but we need to be able to
    // get its output too...
    // Should we bytecode the whole program, or only what needs to run?
    // finding the dependencies can be tricky... 
    PoolAllocator bc_pool;
    bcgen.setPool(&bc_pool);
    bcgen.setInterpreter(this);

    this->bcgen = &bcgen;

    do {
        for (auto dep : overall_deps) {
            processDependencies(dep);
        }
        old_remain = current_remain;
        current_remain = overallDepsItemsSemantic();
        if (current_remain == 0) {
            break;
        } else if (current_remain < old_remain) {
            // if we made progress, clear up errors as they are transient
            reset_errors();
        }
    } while (current_remain < old_remain);

    if (current_remain > 0) {
        Error(nullptr, "Could not process all dependencies\n");
        // This is for debugging, ideally we have errors already
        // printRemainingDependencies();
        success = false;
        return;
    }

    old_remain = overallDepsItemsRun();
    current_remain = old_remain;
    do {
        for (auto dep : overall_deps) {
            processDependenciesRun(root, dep);
        }
        old_remain = current_remain;
        current_remain = overallDepsItemsRun();
        if (current_remain == 0) {
            break;
        } else if (current_remain < old_remain) {
            // if we made progress, clear up errors as they are transient
            reset_errors();
        }
    } while (current_remain < old_remain);

    if (current_remain > 0) {
        Error(nullptr, "Could not process all dependencies related to bytecode\n");
        // This is for debugging, ideally we have errors already
        // printRemainingDependenciesRun();
        success = false;
        return;
    }


    // final validation
    bcgen.validateAllFunctions();
    if (!success) return;

    if (option_printBytecode && success) print_bc_program(bcgen.program);
    this->bcgen = nullptr;
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
#if DEBUG_DEPS
            printWork(work, r);
#endif
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
    if (!stageComplete) {
#if DEBUG_DEPS
        printf("****************** EARLY RETURN, TYPE COULD NOT BE COMPLETED ****************\n");
#endif
        return;
    }

    if (!deps->compute_size.empty()) {
        CPU_SAMPLE("SizeDependencies");

        u32 index = deps->compute_size.active_item;
        bool firstFailure = false;
        for (; index < deps->compute_size.work.size(); index++) {
            auto work = deps->compute_size.work[index];
            bool r = doWorkAST(work);
#if DEBUG_DEPS
            printWork(work, r);
#endif
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
    if (!stageComplete) {
#if DEBUG_DEPS
        printf("****************** EARLY RETURN, SIZE COULD NOT BE COMPLETED ****************\n");
#endif
        return;
    }

    if (!deps->operation_check.empty()) {
        CPU_SAMPLE("CheckDependencies");

        u32 index = deps->operation_check.active_item;
        for (; index < deps->operation_check.work.size(); index++) {
            auto work = deps->operation_check.work[index];
            bool r = doWorkAST(work);
#if DEBUG_DEPS
            printWork(work, r);
#endif
            if (!r) {
                return;
            } else {
                work->action = IA_NOP;
                deps->operation_check.active_item++;
            }
        }
    }
}

void Interpreter::processDependenciesRun(FileAST *root, interp_deps * deps)
{
    CPU_SAMPLE("processDependenciesRun");

    bcgen->startGlobalCompile(root);

    // Bytecode and run stages go hand in hand, we try to process them all
    // start by trying to process bytecode, then run, and repeat
    if (!deps->bytecode_generation.empty()) {
        CPU_SAMPLE("Generate Bytecode");

        u32 index = deps->bytecode_generation.active_item;
        bool firstFailure = false;
        for (; index < deps->bytecode_generation.work.size(); index++) {
            auto work = deps->bytecode_generation.work[index];
            bool r = doBytecode(work);
            if (!r) {
                firstFailure = true;
                success = true;
            } else {
                work->action = IA_NOP;
                if (!firstFailure) deps->bytecode_generation.active_item++;
            }
        }
    }

    if (!deps->run_directives.empty()) {
        CPU_SAMPLE("Process Run Directives");

        bytecode_runner runner;
        runner.program = bcgen->program;

        runner.run_preamble();

        u32 index = deps->run_directives.active_item;
        bool firstFailure = false;
        for (; index < deps->run_directives.work.size(); index++) {
            auto work = deps->run_directives.work[index];
            if (work->action != IA_NOP) {
                auto run = (RunDirectiveAST *)*work->ast;
                if (overallRunDependencies(run) == 0) {
                    runner.run_directive(run);
                    work->action = IA_NOP;
                    if (!firstFailure) deps->run_directives.active_item++;
                } else {
                    firstFailure = true;
                }
            }
        }
    }
}


void Interpreter::printRemainingDependencies()
{
    for (auto dep : overall_deps) {
        for (auto work : dep->resolve_type.work)
        {
            if (work->action != IA_NOP) {
                Error(*work->ast, "%s Unresolved type dependency\n",
                    AstClassTypeToStr((*work->ast)->ast_type));
            }
        }
        for (auto work : dep->compute_size.work)
        {
            if (work->action != IA_NOP) {
                Error(*work->ast, "%s Unresolved size dependency\n",
                    AstClassTypeToStr((*work->ast)->ast_type));
            }
        }
        for (auto work : dep->operation_check.work)
        {
            if (work->action != IA_NOP) {
                Error(*work->ast, "%s Unresolved check dependency\n",
                    AstClassTypeToStr((*work->ast)->ast_type));
            }
        }
    }
}

void Interpreter::printRemainingDependenciesRun()
{
    for (auto dep : overall_deps) {
        for (auto work : dep->bytecode_generation.work)
        {
            if (work->action != IA_NOP) {
                Error(*work->ast, "%s Unresolved bytecode generation\n",
                    AstClassTypeToStr((*work->ast)->ast_type));
            }
        }
        for (auto work : dep->run_directives.work)
        {
            if (work->action != IA_NOP) {
                Error(*work->ast, "%s Unresolved Run Directive\n",
                    AstClassTypeToStr((*work->ast)->ast_type));
            }
        }
    }
}

u64 Interpreter::overallDepsItemsSemantic()
{
    u64 total = 0;
    for (auto dep : overall_deps) {
        total += dep->remaining_items_semantic();
    }
    return total;
}

u64 Interpreter::overallDepsItemsRun()
{
    u64 total = 0;
    for (auto dep : overall_deps) {
        total += dep->remaining_items_run();
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
                if (!(struct_def->struct_type->type_flags & TYPE_FLAG_SIZED)) {
                    // dependencies are not met
                    return false;
                }
                assert((struct_def->struct_type->struct_scope.decls.size() == 0) ||
                    (struct_def->struct_type->size_in_bytes > 0));
                // Go around all the members of the struct and assign relative bc_mem_offset (offset from struct parent)
                u64 bc_offset = 0;
                u32 llvm_index = 0;
                for (auto member : struct_def->struct_type->struct_scope.decls) {
                    member->bc_offset = bc_offset;
                    member->llvm_index = llvm_index++;
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

                bool needs_cast = false;

                switch (decl->definition->ast_type) {
                case AST_FUNCTION_DEFINITION: {
                    FunctionDefinitionAST *fundef = (FunctionDefinitionAST *)decl->definition;
                    TypeAST *dtype = fundef->declaration;
                    TypeAST *stype = decl->specified_type; 
                    
                    if (!compatibleTypes(stype, dtype, needs_cast) || needs_cast) {
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
                    if (!compatibleTypes(stype, dtype, needs_cast) || needs_cast) {
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

                    TypeCheckError err = checkTypesAllowLiteralAndCast(&expr, lhsType, rhsType);
                    if (err != TCH_OK) {
                        char ltype[64] = {}, rtype[64] = {};
                        printTypeToStr(rtype, lhsType);
                        printTypeToStr(ltype, rhsType);

                        switch (err) {
                        case TCH_INCOMPATIBLE_TYPE: {
                            Error(expr, "Incompatible types between defined value type: %s and declared type %s for variable %s\n",
                                rtype, ltype, decl->varname);
                            break;
                        }
                        case TCH_SIGN_MISMATCH: {
                            Error(expr, "Sign mismatch between defined value type: %s and declared type %s for variable %s\n",
                                rtype, ltype, decl->varname);
                            break;
                        }
                        case TCH_LITERAL_NOT_FIT: {
                            Error(expr, "Literal with type: %s could not fit in type %s for variable %s\n",
                                rtype, ltype, decl->varname);
                            break;
                        }
                        case TCH_TYPE_NOT_FIT: {
                            Error(expr, "Initial value type: %s could not fit in declared type %s for variable %s\n",
                                rtype, ltype, decl->varname);
                            break;
                        }
                        default:
                            assert(!"Missing type in error check");
                        }
                        return false;
                    }
                    return true;
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
        u32 accum_size = 0;
        for (auto var : struct_type->struct_scope.decls) {
            if (var->specified_type->size_in_bytes == 0) {
                Error(ast, "Struct %s could not compute size due to member %s\n",
                    struct_type->decl->varname, var->varname);
                return false;
            }
            assert(var->specified_type->size_in_bytes > 0);
            accum_size += var->specified_type->size_in_bytes;
        }
        struct_type->size_in_bytes = accum_size;
        struct_type->type_flags |= TYPE_FLAG_SIZED;
        break;
    }
    case AST_STRUCT_ACCESS: {
        auto sac = (StructAccessAST *)ast;

        if (work->action == IA_RESOLVE_TYPE) {
            if (sac->decl == nullptr) {
                assert(sac->prev);
                TypeAST *enclosingType = getDefinedType(sac->prev);
                if (!enclosingType) {
                    Error(ast, "Could not find reference to %s\n", sac->name);
                    return false;
                }
                if (isTypeArray(enclosingType)) {
                    // Arrays of all type support the struct access .count, some .reserved_size
                    ArrayTypeAST *atype = (ArrayTypeAST *)enclosingType;
                    VariableDeclarationAST *decl = findVariable(sac->name, atype->decls);
                    if (decl == nullptr) {
                        Error(sac, "Variable [%s] cannot be found on Array\n", sac->name);
                        return false;                        
                    }

                    sac->decl = decl;
                    break;
                }

                if (!isTypeStructOrPtr(enclosingType)) {
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
                    // We should have this type, unless there was a missing dependency
                    // in which case we should return and not continue further
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
        } else if (work->action == IA_OPERATION_CHECK) {
            // This phase checks if we can replace the whole StructAccess for a literal
            // For now for Static Arrays, in the future for Enums
            if (sac->next != nullptr) {
                sac->known_val = sac->next->known_val;
            } else {
                TypeAST *enclosingType = getDefinedType(sac->prev);
                if (isTypeArray(enclosingType)) {
                    auto atype = (ArrayTypeAST *)enclosingType;
                    if (isStaticArray(atype)) {
                        if (atype->num_elems == 0) {
                            // break here, we need to compute the num_elems
                            Error(sac, "Could not compute the size of a static array\n");
                            return false;
                        }
                        LiteralAST *lit = new (&pool) LiteralAST;
                        copyASTloc(sac, lit);
                        lit->s = sequence_id++;
                        lit->_u64 = atype->num_elems;
                        lit->typeAST = (DirectTypeAST *)sac->expr_type;
                        assert(sac->expr_type->ast_type == AST_DIRECT_TYPE);
                        lit->expr_type = sac->expr_type;
                        sac->known_val = lit;
                    }
                }
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
                if (!atype) return false;
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
        } else if (work->action == IA_OPERATION_CHECK) {
            // This phase checks if we can replace the whole StructAccess for a literal
            // For now for Static Arrays, in the future for Enums
            if (acc->next != nullptr) {
                acc->known_val = acc->next->known_val;
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
            bool needs_cast = false;
            TypeAST *ret_type = ret_stmt->ret->expr_type;
            FunctionDefinitionAST *fundef = findEnclosingFunction(ret_stmt);
            if (!fundef) {
                Error(ret_stmt, "Return statement not within a function\n");
                return false;
            }
            TypeAST *func_ret_type = fundef->declaration->return_type;
            TypeCheckError err = checkTypesAllowLiteralAndCast(&ret_stmt->ret, func_ret_type, ret_type);
            if (err != TCH_OK) {
                char ltype[64] = {}, rtype[64] = {};
                printTypeToStr(rtype, ret_type);
                printTypeToStr(ltype, func_ret_type);

                switch (err) {
                case TCH_INCOMPATIBLE_TYPE: {
                    Error(ret_stmt, "Incompatible types between the return statement: %s and the return type %s for function %s\n",
                        rtype, ltype, fundef->var_decl->varname);
                    break;
                }
                case TCH_SIGN_MISMATCH: {
                    Error(ret_stmt, "Sign mismatch between the return statement: %s and the return type %s for function %s\n",
                        rtype, ltype, fundef->var_decl->varname);
                    break;
                }
                case TCH_LITERAL_NOT_FIT: {
                    Error(ret_stmt, "Literal on the return statement: %s could not fit in the return type %s for function %s\n",
                        rtype, ltype, fundef->var_decl->varname);
                    break;
                }
                case TCH_TYPE_NOT_FIT: {
                    Error(ret_stmt, "Incompatible types due to size between the return statement: %s and the return type %s for function %s\n",
                        rtype, ltype, fundef->var_decl->varname);
                    break;
                }
                default:
                    assert(!"Missing type in error check");
                }
                return false;
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
                // Function calls are like assignments to new vars, the function arguments are the lhs
                lhsType = fundecl->arguments[i]->specified_type;
                rhsType = funcall->args[i]->expr_type;
                assert(lhsType); assert(rhsType);
                if (isTypeNullPtr(rhsType)) {
                    if (!isTypePointer(lhsType)) {
                        char ltype[64] = {};
                        printTypeToStr(ltype, lhsType);
                        Error(funcall->args[i], "Incompatible types between the provided argument: (null) and the function argument type %s for function %s, argument %d\n",
                            ltype, funcall->function_name, i);
                        return false;
                    }

                    auto nptr = (NullPtrAST *)funcall->args[i];
                    nptr->type_to_null = lhsType;
                    continue;
                }
                TypeCheckError err = checkTypesAllowLiteralAndCast(&funcall->args[i], lhsType, rhsType);
                if (err != TCH_OK) {
                    char ltype[64] = {}, rtype[64] = {};
                    printTypeToStr(ltype, lhsType);
                    printTypeToStr(rtype, rhsType);

                    switch (err) {
                    case TCH_INCOMPATIBLE_TYPE: {
                        Error(funcall->args[i], "Incompatible types between the provided argument: %s and the function argument type %s for function %s, argument %d\n",
                            rtype, ltype, funcall->function_name, i);
                        break;
                    }
                    case TCH_SIGN_MISMATCH: {
                        Error(funcall->args[i], "Sign mismatch between the provided argument: %s and the function argument type %s for function %s, argument %d\n",
                            rtype, ltype, funcall->function_name, i);
                        break;
                    }
                    case TCH_LITERAL_NOT_FIT: {
                        Error(funcall->args[i], "Literal on the provided argument: %s and the function argument type %s for function %s, argument %d\n",
                            rtype, ltype, funcall->function_name, i);
                        break;
                    }
                    case TCH_TYPE_NOT_FIT: {
                        Error(funcall->args[i], "Incompatible types due to size between the provided argument: %s and the function argument type %s for function %s, argument %d\n",
                            rtype, ltype, funcall->function_name, i);
                        break;
                    }
                    default:
                        assert(!"Missing type in error check");
                    }
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
                if (dt->custom_type->size_in_bytes == 0) {
                    // The custom type has not been processed yet and we need to wait
                    return false;
                }
                dt->size_in_bytes = dt->custom_type->size_in_bytes;
                dt->type_flags |= TYPE_FLAG_SIZED;
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
            // @TODO: this will break because on case of new, the array needs not have a defined
            // expression. Only on globals or local variable declaration this is needed
            // this check would be better done at a higher level
            if (isStaticArray(at)) {
                if (!isDefinedExpression(at->num_expr)) {
                    Error(at, "Array needs to have a constant and defined size for number of elements\n");
                    return false;
                }
                assert(at->num_expr);
                if (at->num_expr->expr_type->ast_type != AST_DIRECT_TYPE) {
                    Error(at, "Array needs to have an integer type for their size\n");
                    return false;
                }
                auto dt = (DirectTypeAST *)at->num_expr->expr_type;
                if (dt->basic_type != BASIC_TYPE_INTEGER) {
                    Error(at, "Array needs to have an integer type for their size\n");
                    return false;
                }
                at->num_elems = computeValueInt(at->num_expr);
                if (at->num_elems == 0) {
                    Error(at, "Array cannot have 0 length\n");
                    return false;
                }
                at->size_in_bytes = (u32)at->num_elems * at->array_of_type->size_in_bytes;
            } else {
                if (isDynamicArray(at)) {
                    // Data pointer, count, reserved_size
                    at->size_in_bytes = 8 + 8 + 8; 
                } else {
                    assert(isSizedArray(at));
                    // Data pointer, Total num elems
                    at->size_in_bytes = 8 + 8;
                }
            }
            u64 bc_offset = 0; // for all the cases that matter, we have .data
            u32 llvm_index = 0;
            for(auto d: at->decls) {
                d->bc_offset = bc_offset;
                d->llvm_index = llvm_index++;
                bc_offset += d->specified_type->size_in_bytes;
            }
            at->type_flags |= TYPE_FLAG_SIZED;
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
                if (!id->next->expr_type) return false;
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
            // This phase checks if we can replace the whole StructAccess for a literal
            // For now for Static Arrays, in the future for Enums
            if (id->next != nullptr) {
                id->known_val = id->next->known_val;
            } 

            if (id->known_val != nullptr) {
                *work->ast = id->known_val;
                return true;
            }

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
                dt->s = sequence_id++;
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
                    dt->s = sequence_id++;
                    binop->expr_type = dt;
                    dt->basic_type = lhsDtType->basic_type;
                    dt->size_in_bytes = lhsDtType->size_in_bytes;
                    // The optimization for Literal will require replacement
                } else if (lhsType->ast_type == AST_POINTER_TYPE) {
                    PointerTypeAST *pt = new (&pool) PointerTypeAST;
                    auto lhsPtType = (PointerTypeAST *)lhsType;

                    copyASTloc(binop, pt);
                    pt->s = sequence_id++;
                    binop->expr_type = pt;
                    pt->points_to_type = lhsPtType->points_to_type;
                    pt->size_in_bytes = 8;
                }
            }
        } else if (work->action == IA_COMPUTE_SIZE) {

        } else if (work->action == IA_OPERATION_CHECK) {

            bool needs_cast = false;
            if (isNullLiteral(binop->lhs)) {
                auto nptr = (NullPtrAST *)binop->rhs;

                // special case of Null being on the left hand side
                if (!isBoolOperator(binop->op)) {
                    Error(binop, "Null can only be used on boolean operatorsm found on: %s\n",
                        TokenTypeToCOP(binop->op));
                    return false;
                }

                if (isNullLiteral(binop->rhs)) {
                    // Two null can be compared, not useful but valid
                    return true;
                }

                if (!compatibleTypes(lhsType, rhsType, needs_cast)) {
                    char ltype[64] = {}, rtype[64] = {};
                    printTypeToStr(ltype, lhsType);
                    printTypeToStr(rtype, rhsType);
                    Error(binop, "Incompatible types during binary operation %s: %s and %s\n",
                        TokenTypeToCOP(binop->op),
                        ltype, rtype);
                    return false;
                }
                assert(!needs_cast);

                nptr->type_to_null = rhsType;
                return true;
            }
            if (lhsType->ast_type == AST_POINTER_TYPE) {
                /* Pointers are a special case
                   We allow The following operations:
                   PTR == PTR (all the boolean operators)
                   PTR + / - UINT/SINT  Only add or substract ints
                */

                if (isBoolOperator(binop->op)) {
                    if (!compatibleTypes(lhsType, rhsType, needs_cast)) {
                        char ltype[64] = {}, rtype[64] = {};
                        printTypeToStr(ltype, lhsType);
                        printTypeToStr(rtype, rhsType);
                        Error(binop, "Incompatible types during binary operation %s: %s and %s\n",
                            TokenTypeToCOP(binop->op),
                            ltype, rtype);
                        return false;
                    }
                    assert(!needs_cast);

                    // Special case for null pointers, we need to propagate
                    // the type. Only makes sense on boolean operators
                    if (isNullLiteral(binop->rhs)) {
                        auto nptr = (NullPtrAST *)binop->rhs;
                        nptr->type_to_null = lhsType;
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

            
            // TODO: extend this to compare function pointers (should be valid)
            // I am not sure on comparing arrays or structs, in general that should not be allowed

            // we better be a direct type if we get to this point
            assert(lhsType->ast_type == AST_DIRECT_TYPE);

            if (!isBoolOperator(binop->op) || 
                (binop->op == TK_EQ) ||
                (binop->op == TK_NEQ)) {
                // Non boolean operators demand types that match
                // same if we check for equality
                if (!compatibleTypes(lhsType, rhsType, needs_cast)) {
                    char ltype[64] = {}, rtype[64] = {};
                    printTypeToStr(ltype, lhsType);
                    printTypeToStr(rtype, rhsType);
                    Error(binop, "Incompatible types during binary operation %s: %s and %s\n",
                        TokenTypeToCOP(binop->op),
                        ltype, rtype);
                    return false;
                }
                assert(!needs_cast);
            } else {
                // Here we have a comparison operator
                // @TODO: be a bit lenient on comparisons..., with auto cast
                // For now the types have to match and they need to be INTEGER or FLOAT only
                if (!compatibleTypes(lhsType, rhsType, needs_cast)) {
                    char ltype[64] = {}, rtype[64] = {};
                    printTypeToStr(ltype, lhsType);
                    printTypeToStr(rtype, rhsType);
                    Error(binop, "Incompatible types during binary operation %s: %s and %s\n",
                        TokenTypeToCOP(binop->op),
                        ltype, rtype);
                    return false;
                }
                assert(!needs_cast);
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
            if (isNullLiteral(unop->expr)) {
                Error(unop, "Unary operation [%s] is not allowed on a null pointer\n", TokenTypeToCOP(unop->op));
                return false;
            }

            TypeAST *type = unop->expr->expr_type;
            // This could happen because there is a lingering error, stop here. 
            if (type == nullptr) return false;

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
                    dtype->s = sequence_id++;
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
                dt->s = sequence_id++;
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
                pt->s = sequence_id++;
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

            if (isNullLiteral(assign->rhs) && isTypePointer(lhsType)) {
                auto nptr = (NullPtrAST *)assign->rhs;
                nptr->type_to_null = lhsType;
                return true;
            }

            TypeCheckError err = checkTypesAllowLiteralAndCast(&assign->rhs, lhsType, rhsType);
            if (err != TCH_OK) {
                char ltype[64] = {}, rtype[64] = {};
                printTypeToStr(rtype, lhsType);
                printTypeToStr(ltype, rhsType);

                switch (err) {
                case TCH_INCOMPATIBLE_TYPE: {
                    Error(assign, "Incompatible types on assignment between the righ hand side: %s and left hand side type %s\n",
                        rtype, ltype);
                    break;
                }
                case TCH_SIGN_MISMATCH: {
                    Error(assign, "Sign mismatch  on assignment between the righ hand side: %s and left hand side type %s\n",
                        rtype, ltype);
                    break;
                }
                case TCH_LITERAL_NOT_FIT: {
                    Error(assign, "Literal does not fit on assignment: %s (%d bytes) and left hand side type %s (%d bytes)\n",
                        rtype, rhsType->size_in_bytes, ltype, lhsType->size_in_bytes);
                    break;
                }
                case TCH_TYPE_NOT_FIT: {
                    Error(assign, "Right hand side does not fit on assignment: %s (%d bytes) and left hand side type %s (%d bytes)\n",
                        rtype, rhsType->size_in_bytes, ltype, lhsType->size_in_bytes);
                    break;
                }
                default:
                    assert(!"Missing type in error check");
                }
                return false;
            }

            return true;
            // one of the hard things here is what to do on complex expressions with literals, promote?

        } else {
            assert(!"Unknown dependency work type");
        }
        break;
    }
    case AST_NEW: {
        auto nast = (NewAllocAST *)ast;
        if (work->action == IA_RESOLVE_TYPE) {
            TypeAST *type = nast->type;
            assert(type);

            PointerTypeAST *pt = new (&pool) PointerTypeAST;
            copyASTloc(nast, pt);
            pt->s = sequence_id++;
            pt->points_to_type = type;
            pt->size_in_bytes = 8;

            nast->expr_type = pt;

            if (isTypeArray(type)) {
                auto at = (ArrayTypeAST *)type;
                // Static arrays are returned as Sized array (but with count already)
                at->array_type = ArrayTypeAST::SIZED_ARRAY;
            } else { 
            }
        } else if (work->action == IA_COMPUTE_SIZE) {
            
        } else if (work->action == IA_OPERATION_CHECK) {
            // Decide on what checks to do for a new call, if any
        } else {
            assert(!"Unimplemented");
        } 
        break;
    }
    case AST_RUN_DIRECTIVE: {
        auto run = (RunDirectiveAST *)ast;
        if (work->action == IA_RESOLVE_TYPE) {
            TypeAST *type = run->expr->expr_type;
            assert(type);

            run->expr_type = type;

            if (!isVoidType(run->expr_type)) {
                if (!isTypeRunSupported(run->expr_type)) {
                    assert(!"Run directive with non void type return not supported yet");
                }
                // Allocate a new AST for the run expression, assume it is a literal for now
                LiteralAST *lit = new (&pool) LiteralAST();
                run->new_ast = lit;
                copyASTloc(run, lit);
                lit->s = sequence_id++;

                assert(run->expr_type->ast_type == AST_DIRECT_TYPE);
                lit->typeAST = (DirectTypeAST *)run->expr_type;
                lit->_u64 = 0;
            }
        } else if (work->action == IA_OPERATION_CHECK) {
            // Run directives can only depend on constants/literals
            auto res = enforceRunDirectiveConstraints(run->expr);
            return res;
        } else {
            assert(!"Unimplemented");
        } 
        break;
    }
    case AST_FOR_STATEMENT: {
        auto forst = (ForStatementAST *)ast;
        if (forst->is_array) {
            //traversePostfixAST(PPC(forst->arr), deps);
        } else {
            //traversePostfixAST(PPC(forst->start), deps);
            //traversePostfixAST(PPC(forst->end), deps);
        }

        //addTypeWork(&pool, astp, deps);
        //addCheckWork(&pool, astp, deps);

        //traversePostfixAST(PPC(forst->it), deps);
        //traversePostfixAST(PPC(forst->it_index), deps);


        if (work->action == IA_RESOLVE_TYPE) {
            // Add the extra it, it_index variables to the scope
            // with the right typing

            bool needs_cast;

            if (forst->is_array) {
                if (!isTypeArray(forst->arr->expr_type)) {
                    char atype[64] = {};
                    printTypeToStr(atype, forst->arr->expr_type);
                    Error(forst, "The for loop needs a variable of type array, but found variable %s of type %s\n",
                        forst->arr->name, atype);
                    return false;
                }
            } else {
                if (forst->is_it_ptr) {
                    Error(forst, "Pointer iterator declaration (%s) is not allowed on range iterations\n", forst->it->name);
                    return false;
                }

                if (!compatibleTypes(forst->start->expr_type, forst->end->expr_type, needs_cast)) {
                    Error(forst, "Start and End expressions are not compatible");
                    return false;
                }
            }


            // Here we should also create the two iterators, but being careful with type
            // and maybe names (as they might come from the for)
            const char *it_name;
            if (forst->it != nullptr) {
                it_name = forst->it->name;
            } else {
                it_name = "it";
            }

            TypeAST *decl_type = nullptr;
            VariableDeclarationAST *decl = nullptr;

            if (forst->is_array) {
                assert(isTypeArray(forst->arr->expr_type));
                auto arr_type = (ArrayTypeAST *)forst->arr->expr_type;
                decl_type = arr_type->array_of_type;

                if (forst->is_it_ptr) {
                    assert(forst->it != nullptr);
                    decl = createDeclarationPtr(it_name, decl_type, nullptr);
                } else {
                    decl = createDeclaration(it_name, decl_type, nullptr);
                }
            } else {
                decl_type = forst->start->expr_type;
                decl = createDeclaration(it_name, decl_type, forst->start);
            }

            copyASTloc(forst, decl);
            decl->flags |= DECL_FLAG_IS_LOCAL_VARIABLE;
            decl->scope = &forst->for_scope;
            // There is no error check here in case the 'it' is overlapping some variable
            // This is a minor improvement, likely a @TODO
            forst->for_scope.decls.push_back(decl);
            if (forst->it == nullptr) {
                forst->it = createIdentifier(it_name, decl);
            } else {
                forst->it->scope = decl->scope;
            }


            const char *it_index_name;
            if (forst->it_index == nullptr) {
                it_index_name = "it_index";
            } else {
                it_index_name = forst->it_index->name;
            }
            decl = createDeclarationUInt(it_index_name, 0, forst);
            decl->flags |= DECL_FLAG_IS_LOCAL_VARIABLE;
            decl->scope = &forst->for_scope;
            forst->for_scope.decls.push_back(decl);

            if (forst->it_index == nullptr) {
                forst->it_index = createIdentifier(it_index_name, decl);
            } else {
                forst->it_index->scope = decl->scope;
            }
        } else if (work->action == IA_OPERATION_CHECK) {

            // Possible improvement: if start and end are literals of known value, check that 
            // their value is in increasing order

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

bool Interpreter::doBytecode(interp_work * work)
{
    // null pointer is valid, like if a type does not exist 
    if (work->action == IA_NOP) return true;
    BaseAST *ast = *(work->ast);

    switch (ast->ast_type) {
    case AST_VARIABLE_DECLARATION: {
        auto decl = (VariableDeclarationAST *)ast;
        assert(decl->flags & DECL_FLAG_IS_GLOBAL_VARIABLE);
        // Check for any errors and thus skip
        // The function owns restoring the watermark
        bcgen->initializeGlobalVariable(decl);
        if (!success) return false;
        break;
    }
    case AST_RUN_DIRECTIVE: {
        auto run = (RunDirectiveAST *)ast;
        if (!run->generated) bcgen->generate_run_directive(run);
        if (!success) return false;
        break;
    }
    default:
        assert(false);
        break;
    }
    return true;
}
