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

static VariableDeclarationAST *findVariable(TextType name, Scope *scope)
{
    // trivial recursive case, could not be found
    if (scope == nullptr) return nullptr;

    for (auto d : scope->decls) {
        if (!strcmp(name, d->varname)) return d;
    }
    return findVariable(name, scope->parent);
}

void copyASTloc(BaseAST *src, BaseAST *dst)
{
    dst->filename = src->filename;
    dst->line_num = src->line_num;
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

    if (decl->specified_type && decl->specified_type->ast_type != AST_FUNCTION_TYPE) {
        Error(a, "Cannot perform a function call on a variable [%s] that is not a function\n",
            a->function_name);
        return nullptr;
    }
    return decl;
}


TypeAST * Interpreter::deduceType(ExpressionAST *expr)
{
    switch (expr->ast_type) {
    case AST_FUNCTION_CALL: {
        FunctionCallAST *a = (FunctionCallAST *)expr;
        VariableDeclarationAST *decl = validateFunctionCall(a);
        if (!decl) return nullptr;

        // do not recurse on inferring types as this could cause infinite recursion
        if (decl->specified_type == nullptr) {
            return nullptr;
        }

        assert(decl->definition->ast_type == AST_FUNCTION_DEFINITION);
        a->fundef = (FunctionDefinitionAST *)decl->definition;
        FunctionTypeAST *fundecl = (FunctionTypeAST *)decl->specified_type;
        if (!fundecl->return_type) {
            Error(expr, "Cannot use the return value of a void function [%s]\n", a->function_name);
        }
        expr->expr_type = fundecl->return_type;
        return fundecl->return_type;
    }
    case AST_IDENTIFIER: {
        IdentifierAST *a = (IdentifierAST *)expr;
        VariableDeclarationAST *decl = validateVariable(a);

        // do not recurse on inferring types as this could cause infinite recursion
        if (!decl) return nullptr;

        a->decl = decl;
        expr->expr_type = decl->specified_type;
        return decl->specified_type;
    }
    case AST_LITERAL: {
        auto lt = (LiteralAST *)expr;
        expr->expr_type = &lt->typeAST;
        return &lt->typeAST;
    }
    case AST_BINARY_OPERATION: {
        // @TODO: the type should be a combination of both lhs and rhs?
        // @TODO: The type here should also depend on the operator
        TypeAST *type = deduceType(((BinaryOperationAST *)expr)->lhs);
        expr->expr_type = type;
        return type;
    }
    case AST_UNARY_OPERATION: {
        auto un = (UnaryOperationAST *)expr;
        // @TODO: Use the operation in `un` to choose the type
        TypeAST *type = deduceType(un->expr);
        expr->expr_type = type;
        return type;
    }
    case AST_RUN_DIRECTIVE: {
        auto run = (RunDirectiveAST *)expr;
        // Run directives do not change the type of whatever they run, pass through
        TypeAST *type = deduceType(run->expr);
        expr->expr_type = type;
        return type;
    }
    default:
        assert("We should never be here, we could not parse this type\n");
    }
    return nullptr;
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

u32 Interpreter::process_scope_variables(Scope * scope)
{
    u32 untyped_vars = 0;
    for (auto &decl : scope->decls) {
        if (decl->specified_type == nullptr) {
            if (!infer_types(decl)) {
                untyped_vars++;
            }
        }
    }
    return untyped_vars;
}

void Interpreter::process_all_scope_variables(Scope *scope)
{
    u32 untyped_vars, prev_vars = 0;

    do {
        untyped_vars = process_scope_variables(scope);
        if (prev_vars == 0) {
            prev_vars = untyped_vars;
        } else if ((prev_vars == untyped_vars) && prev_vars > 0) {
            return;
        }
   } while (success && (untyped_vars > 0));
}

bool Interpreter::infer_types(VariableDeclarationAST *decl)
{
    if (decl->specified_type) return true;
    current_identifier = decl->varname;

    assert(decl->definition); // if we do not have a type we must have something to compare against
    switch (decl->definition->ast_type) {
    case AST_FUNCTION_DEFINITION: {
        FunctionDefinitionAST *fundef = (FunctionDefinitionAST *)decl->definition;
        decl->specified_type = fundef->declaration;
        decl->flags |= DECL_FLAG_HAS_BEEN_INFERRED;
        assert(decl->specified_type->size_in_bits);
        return true;
    }
    default: {
        // expect this to be an expression, and the expression type needs to be deduced
        // operation, literal, function call, etc
        TypeAST *t = deduceType((ExpressionAST *)decl->definition);
        if (t != nullptr) {
            decl->specified_type = t;
            decl->flags |= DECL_FLAG_HAS_BEEN_INFERRED;
            assert(t->size_in_bits);
            return true;
        }
        break;
    }
    }
    return false;
}

bool Interpreter::checkVariablesInExpression(ExpressionAST *expr)
{
    switch (expr->ast_type) {
    case AST_FUNCTION_CALL: {
        FunctionCallAST *a = (FunctionCallAST *)expr;
        VariableDeclarationAST *decl = findVariable(a->function_name, a->scope);

        if (decl == nullptr) {
            Error(expr, "Function [%s] could not be found on this scope\n", a->function_name);
        }

        bool ret = true;

        for (auto arg : a->args) {
            if (!checkVariablesInExpression(arg)) ret = false;
        }

        return ret;
    }
    case AST_IDENTIFIER: {
        IdentifierAST *a = (IdentifierAST *)expr;
        VariableDeclarationAST *decl = findVariable(a->name, a->scope);

        if (decl == nullptr) {
            Error(expr, "Variable [%s] could not be found on this scope\n", a->name);
            return false;
        }

        if ((decl->filename == expr->filename) &&
            (decl->line_num > expr->line_num) &&
            !(decl->flags & DECL_FLAG_IS_CONSTANT))
        {
            Error(expr, "The variable [%s] used in this declaration appears after the current declaration, this is only allowed for constants\n",
                a->name);
        }

        return true;
    }
    case AST_LITERAL: {
        return true;
    }
    case AST_BINARY_OPERATION: {
        return checkVariablesInExpression(((BinaryOperationAST *)expr)->lhs) &&
            checkVariablesInExpression(((BinaryOperationAST *)expr)->rhs);
    }
    case AST_UNARY_OPERATION: {
        auto un = (UnaryOperationAST *)expr;
        return checkVariablesInExpression(un->expr);
    }
    case AST_RUN_DIRECTIVE: {
        auto run = (RunDirectiveAST *)expr;
        return checkVariablesInExpression(run->expr);
        break;
    }
    default:
        assert("We should never be here, we could not parse this type\n");
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
        VariableDeclarationAST *decl = findVariable(a->name, a->scope);

        if (decl == nullptr) {
            Error(expr, "Variable [%s] could not be found on this scope\n", a->name);
            return false;
        }

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
    default:
        assert("We should never be here, we could not parse this type\n");
    }
    return false;
}

/*
    Responsibilities of traverseAST for Expressions

    - Are variables declared in this scope?
    - are the types of operands compatible?
    - are function calls correct (name, ret type, arg types)

    Are we doing duplicate work when we recurse?
*/
void Interpreter::traverseAST(ExpressionAST *expr)
{
    if (!success) return;
    switch (expr->ast_type) {
    case AST_ASSIGNMENT: {
        auto assgn = (AssignmentAST *)expr;
        // Errors to check:
        // any variable used in the assignment needs to be declared in the scope. To be checked here or recursively?
        checkVariablesInExpression(assgn->lhs);

        if (!success) {
            return;
        }

        // lhs cannot be const (or a literal, same thing)
        if (isConstExpression(assgn->lhs)) {
            Error(assgn, "The left hand side of an assignment must be an l-value\n");
        }

        // lhs and rhs need to have the same type
        TypeAST *lhsType, *rhsType;
        lhsType = deduceType(assgn->lhs);
        rhsType = deduceType(assgn->rhs);
        if (!compatibleTypes(lhsType, rhsType)) {
            char ltype[64] = {}, rtype[64] = {};
            printTypeToStr(ltype, lhsType);
            printTypeToStr(rtype, rhsType);
            Error(assgn, "Incompatible types during assignment: %s and %s\n", ltype, rtype);
        }

        // @TODO: check for width of operands (in case of literals), like assigning 512 to an u8.
        // one of the hard things here is what to do on complex expressions with literals, promote?

        // are there any checks to do on the left hand side?
        traverseAST(assgn->rhs);
        break;
    }
    case AST_FUNCTION_CALL: {
        // normal checks are already satisfied on type deduction:
        // [DONE] check that the function being called exists, and that it is a function
        // [DONE] for each argument, check that the type of the argument matches that of the parameter
        FunctionCallAST *funcall = (FunctionCallAST *)expr;
        VariableDeclarationAST *decl = validateFunctionCall(funcall);
        if (!decl) return;
        assert(isFunctionDeclaration(decl));
        FunctionTypeAST *fundecl = (FunctionTypeAST *)decl->specified_type;
        if (fundecl->hasVariableArguments) {
            // for variable number of argument functions we only need to check a lower bound
            if (fundecl->arguments.size() > funcall->args.size()) {
                Error(funcall, "Function %s called with %d arguments but it expects at least %d\n",
                    funcall->function_name, funcall->args.size(), fundecl->arguments.size());
                return;
            }
        } else {
            if (fundecl->arguments.size() != funcall->args.size()) {
                Error(funcall, "Function %s called with %d arguments but it expects %d\n",
                    funcall->function_name, funcall->args.size(), fundecl->arguments.size());
                return;
            }
        }

        for (u32 i = 0; i < fundecl->arguments.size(); i++) {
            TypeAST *lhsType, *rhsType;
            lhsType = deduceType(funcall->args[i]);
            rhsType = fundecl->arguments[i]->specified_type;
            if (!success) return;
            if (!compatibleTypes(lhsType, rhsType)) {
                char ltype[64] = {}, rtype[64] = {};
                printTypeToStr(ltype, lhsType);
                printTypeToStr(rtype, rhsType);
                Error(funcall, "Incompatible types during function call: provided: %s and expected: %s\n",
                    ltype, rtype);
                return;
            }
        }
        break;
    }
    case AST_RUN_DIRECTIVE: {
        // simple passthrough check
        auto run = (RunDirectiveAST *)expr;
        traverseAST(run->expr);
        break;
    }
    case AST_BINARY_OPERATION: {
        auto binop = (BinaryOperationAST *)expr;
        TypeAST *lhsType, *rhsType;
        lhsType = deduceType(binop->lhs);
        rhsType = deduceType(binop->rhs);
        // @TODO: Check that the operator matches the types of rhs, lhs
        if (!compatibleTypes(lhsType, rhsType)) {
            char ltype[64] = {}, rtype[64] = {};
            printTypeToStr(ltype, lhsType);
            printTypeToStr(rtype, rhsType);
            Error(binop, "Incompatible types during binary operation %s: %s and %s\n",
                  TokenTypeToCOP(binop->op),
                  ltype, rtype);
        }
        traverseAST(binop->lhs);
        traverseAST(binop->rhs);
        break;
    }
    case AST_UNARY_OPERATION: {
        // @TODO: Check that the operator makes sense with the inner type
        assert(!"Not implemented traverseAST checks for Unary Operation");
        break;
    }
    case AST_IDENTIFIER: {
        assert(!"Identifier not supported for traverseAST yet");
        break;
    }
    case AST_LITERAL:
        // Nothing to check here
        break;
    default:
        assert(!"Missing AST support types");
    }
}

void Interpreter::perform_bytecode(FileAST * root)
{
    bytecode_generator bcgen;
    // @TODO : should the bytecode have its own pool? 
    // makes sense since it is emphymeral, but we need to be able to
    // get its output too...
    PoolAllocator bc_pool;   
    bcgen.setPool(&bc_pool);
    bytecode_program *bp = bcgen.compileToBytecode(root);
    
    print_bc_program(bp);
}

void Interpreter::traverseAST(StatementBlockAST *root)
{
    if (root == nullptr) return; // this happens for foreign functions
    process_all_scope_variables(&root->block_scope);

    if (!success) return;

    Scope *previous_scope = current_scope;
    current_scope = &root->block_scope;
    for (auto stmt : root->statements) {
        if (stmt->ast_type == AST_VARIABLE_DECLARATION) {
            auto decl = (VariableDeclarationAST *)stmt;
            if (decl->definition) {
                if (decl->definition->ast_type == AST_FUNCTION_DEFINITION) {
                    FunctionDefinitionAST *fundef = (FunctionDefinitionAST *)decl->definition;
                    traverseAST(fundef->function_body);
                } else {
                    traverseAST((ExpressionAST *)decl->definition);
                }
            }
        } else if (stmt->ast_type == AST_STATEMENT_BLOCK) {
            traverseAST((StatementBlockAST *)stmt);
        } else if ((stmt->ast_type == AST_ASSIGNMENT) ||
            (stmt->ast_type == AST_FUNCTION_CALL)) {
            traverseAST((ExpressionAST *)stmt);
        } else if (stmt->ast_type == AST_RETURN_STATEMENT) {
            auto ret_stmt = (ReturnStatementAST *)stmt;
            // we need to check the return expression itself
            traverseAST(ret_stmt->ret);
            
            // next we need to type check the return expression
            // with the enclosing function (!!)
            assert(!"Need to extend the traverseAST for return statements");
        } else {
            assert(!"Unhandled traverseAST check in statement block");
        }
    }
    current_scope = previous_scope;
}

void Interpreter::printErrors()
{
    for (auto err : errors) {
        printf("%s", err);
    }
}

void Interpreter::traverseAST(FileAST *root)
{
    process_all_scope_variables(&root->global_scope);
    current_scope = &root->global_scope;
    for (auto &ast : root->items) {
        switch (ast->ast_type) {
        case AST_VARIABLE_DECLARATION: {
            auto decl = (VariableDeclarationAST *)ast;
            if (isFunctionDeclaration(decl)) {
                FunctionDefinitionAST *fundef = (FunctionDefinitionAST *)decl->definition;
                traverseAST(fundef->function_body);
            }
            break;
        }
        case AST_RUN_DIRECTIVE: {
            // @TODO: implement checks for run directive
            // At some point we want to execute the run directive, after checks
            // and sizes
            break;
        }
        default:
            assert(!"Unsupported type on top level expression");
        }
    }
    if (!success) return;

    perform_bytecode(root);
}
