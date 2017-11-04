#include "Interpreter.h"
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include "TextType.h"

#ifndef WIN32
# define sprintf_s sprintf
# define vsprintf_s vsnprintf
#endif

void Interpreter::Error(BaseAST *ast, const char *msg, ...)
{
    va_list args;
    u32 off = sprintf_s(errorString, "%s(%d): error : ", ast->filename, ast->line_num);

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

TypeAST * Interpreter::deduceType(ExpressionAST *expr)
{
    switch (expr->ast_type) {
    case AST_FUNCTION_CALL: {
        FunctionCallAST *a = (FunctionCallAST *)expr;
        VariableDeclarationAST *decl = findVariable(a->function_name, a->scope);

        if (decl == nullptr) {
            Error(expr, "Function [%s] could not be found on this scope\n", a->function_name);
            return nullptr;
        }
        // do not recurse on inferring types as this could cause infinite recursion
        if (decl->specified_type == nullptr) {
            return nullptr;
        }

        if (decl->specified_type->ast_type != AST_FUNCTION_TYPE) {
            Error(expr, "Cannot perform a function call on a variable [%s] that is not a function\n",
                a->function_name);
            return nullptr;
        }
        FunctionTypeAST *fundecl = (FunctionTypeAST *)decl->specified_type;
        if (!fundecl->return_type) {
            Error(expr, "Cannot use the return value of a void function [%s]\n", a->function_name);
        }
        return fundecl->return_type;
    }
    case AST_IDENTIFIER: {
        IdentifierAST *a = (IdentifierAST *)expr;
        VariableDeclarationAST *decl = findVariable(a->name, a->scope);

        if (decl == nullptr) {
            Error(expr, "Variable [%s] could not be found on this scope\n", a->name);
            return nullptr;
        }

        if ((decl->filename == expr->filename) &&
            (decl->line_num > expr->line_num) &&
            !(decl->flags & DECL_FLAG_IS_CONSTANT))
        {
            Error(expr, "The variable [%s] used in this declaration appears after the current declaration, this is only allowed for constants\n",
                a->name);
            return nullptr;
        }

        // do not recurse on inferring types as this could cause infinite recursion
        return decl->specified_type;
    }
    case AST_CONSTANT_NUMBER: {
        ConstantNumberAST *cons = (ConstantNumberAST *)expr;
        DirectTypeAST *direct_type = new DirectTypeAST();
        copyASTloc(expr, direct_type);
        direct_type->type = cons->type;
        return direct_type;
    }
    case AST_CONSTANT_STRING: {
        DirectTypeAST *direct_type = new DirectTypeAST();
        copyASTloc(expr, direct_type);
        direct_type->type = BASIC_TYPE_STRING;
        return direct_type;
    }
    case AST_BINARY_OPERATION: {
        return deduceType(((BinaryOperationAST *)expr)->lhs);
    }
    default:
        assert("We should never be here, we could not parse this type\n");
    }
    return nullptr;
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
    assert(decl->definition); // if we do not have a type we must have something to compare against
    switch (decl->definition->ast_type) {
    case AST_FUNCTION_DEFINITION: {
        FunctionDefinitionAST *fundef = (FunctionDefinitionAST *)decl->definition;
        decl->specified_type = fundef->declaration;
        decl->flags |= DECL_FLAG_HAS_BEEN_INFERRED;
        return true;
    }
    default: {
        // expect this to be an expression, and the expression type needs to be deduced
        // operation, literal, function call, etc
        TypeAST *t = deduceType((ExpressionAST *)decl->definition);
        if (t != nullptr) {
            decl->specified_type = t;
            decl->flags |= DECL_FLAG_HAS_BEEN_INFERRED;
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
    case AST_CONSTANT_NUMBER: {
        return true;
    }
    case AST_CONSTANT_STRING: {
        return true;
    }
    case AST_BINARY_OPERATION: {
        return checkVariablesInExpression(((BinaryOperationAST *)expr)->lhs) &&
            checkVariablesInExpression(((BinaryOperationAST *)expr)->rhs);
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
    case AST_CONSTANT_NUMBER: {
        return true;
    }
    case AST_CONSTANT_STRING: {
        return true;
    }
    case AST_BINARY_OPERATION: {
        return true;
    }
    default:
        assert("We should never be here, we could not parse this type\n");
    }
    return false;
}

void Interpreter::traverseAST(StatementBlockAST *root)
{
    process_all_scope_variables(&root->scope);

    if (!success) return;

    Scope *previous_scope = current_scope;
    current_scope = &root->scope;
    for (auto stmt : root->statements) {
        if (stmt->ast_type == AST_VARIABLE_DECLARATION) {
            auto decl = (VariableDeclarationAST *)stmt;
            if ((decl->definition) && (decl->definition->ast_type == AST_FUNCTION_DEFINITION)) {
                FunctionDefinitionAST *fundef = (FunctionDefinitionAST *)decl->definition;
                traverseAST(fundef->function_body);
            }
        } else if (stmt->ast_type == AST_STATEMENT_BLOCK) {
            traverseAST((StatementBlockAST *)stmt);
        } else if (stmt->ast_type == AST_ASSIGNMENT) {
            auto assgn = (AssignmentAST *)stmt;
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
            // check for width of operands (in case of literals), like assigning 512 to an u8.
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
    process_all_scope_variables(&root->scope);
    current_scope = &root->scope;
    for (auto &decl : root->items) {
        if ((decl->definition) && (decl->definition->ast_type == AST_FUNCTION_DEFINITION)) {
            FunctionDefinitionAST *fundef = (FunctionDefinitionAST *)decl->definition;
            traverseAST(fundef->function_body);
        }
    }
}
