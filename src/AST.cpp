#include "AST.h"
#include <stdio.h>

const char *BasicTypeToStr(const DirectTypeAST *t)
{
    switch (t->basic_type)
    {
    case BASIC_TYPE_BOOL:  return "bool";
    case BASIC_TYPE_STRING:  return "string";
    case BASIC_TYPE_INTEGER: {
        if (t->isSigned) {
            switch (t->size_in_bits) {
            case 8: return "s8";
            case 16: return "s16";
            case 32: return "s32";
            default:
            case 64: return "s64";
            }
            assert(false);
            return "UNKNOWN";
        } else {
            switch (t->size_in_bits) {
            case 8: return "u8";
            case 16: return "u16";
            case 32: return "u32";
            default:
            case 64: return "u64";
            }
            assert(false);
            return "UNKNOWN";
        }
    }
    case BASIC_TYPE_FLOATING: 
        if (t->size_in_bits == 32) return "f32";
        if (t->size_in_bits == 64) return "f64";
        return "f64";
        // assert(false);
        return "UNKNOWN";
    }
    return "UNKNOWN";
}

static void printDeclarationASTFlags(u32 flags)
{
    if (flags & DECL_FLAG_HAS_BEEN_GENERATED) {
        printf("DECL_FLAG_HAS_BEEN_GENERATED ");
    }
    if (flags & DECL_FLAG_HAS_BEEN_INFERRED) {
        printf("DECL_FLAG_HAS_BEEN_INFERRED ");
    }
    if (flags & DECL_FLAG_IS_CONSTANT) {
        printf("DECL_FLAG_IS_CONSTANT ");
    }
}

void printAST(const BaseAST *ast, int ident)
{
    if (ast == nullptr) return;

    switch (ast->ast_type) {
    case AST_LITERAL: {
        const LiteralAST *c = (const LiteralAST *)ast;
        printf("%*sLiteralAST type: %s", ident, "", 
            BasicTypeToStr(&c->typeAST));
        switch (c->typeAST.basic_type)
        {
        case BASIC_TYPE_FLOATING:
            printf(" %f", c->_f64);
            break;
        case BASIC_TYPE_BOOL:
            if (c->_bool) printf(" true");
            else printf(" false");
            break;
        case BASIC_TYPE_STRING:
            printf(" %s", c->str);
            break;
        case BASIC_TYPE_INTEGER:
            // for ease of operation, everything else is assumed to be an integer
            if (c->typeAST.isSigned) {
                printf(" %lld", c->_s64);
            } else {
                printf(" %llu", c->_u64);
            }
            break;
        default:
            assert(false);
        }
        printf("\n");
        break;
    }
    case AST_BINARY_OPERATION: {
        const BinaryOperationAST *b = (const BinaryOperationAST *)ast;
        printf("%*sBinOpAST op: %s\n", ident, "", TokenTypeToStr(b->op));
        printf("%*s LHS:\n", ident, "");
        printAST(b->lhs, ident + 3);
        printf("%*s RHS:\n", ident, "");
        printAST(b->rhs, ident + 3);
        break;
    }
    case AST_ASSIGNMENT: {
        const AssignmentAST *a = (const AssignmentAST *)ast;
        printf("%*sAssignAST op: %s\n", ident, "", TokenTypeToStr(a->op));
        printf("%*s LHS:\n", ident, "");
        printAST(a->lhs, ident + 3);
        printf("%*s RHS:\n", ident, "");
        printAST(a->rhs, ident + 3);
        break;
    }
    case AST_VARIABLE_DECLARATION: {
        const VariableDeclarationAST *a = (const VariableDeclarationAST *)ast;
        printf("%*sDeclAST varname: [%s] flags: ", ident, "", a->varname);
        printDeclarationASTFlags(a->flags);     
        printf("\n%*s SpecifiedType: ", ident, "");
        if (a->specified_type) {
            printf("\n");
            printAST(a->specified_type, ident + 3);
        } else {
            printf(" NONE\n");
        }
        printf("%*s DefinitionAST: ", ident, "");
        if (a->definition) {
            printf("\n");
            printAST(a->definition, ident + 3);
        } else {
            printf(" NONE\n");
        }
        break;
    }
    case AST_DIRECT_TYPE: {
        const DirectTypeAST *a = (const DirectTypeAST *)ast;
        printf("%*sDirectTypeAST %s\n", ident, "", BasicTypeToStr(a)); 
        break;
    }
    case AST_ARGUMENT_DECLARATION: {
        const ArgumentDeclarationAST *a = (const ArgumentDeclarationAST *)ast;
        printf("%*sArgumentDeclarationAST name: %s\n", ident, "", a->name);
        printAST(a->type, ident + 3);
        break;
    }
    case AST_FUNCTION_TYPE: {
        const FunctionTypeAST *a = (const FunctionTypeAST *)ast;
        printf("%*sFunctionDeclarationAST with %d arguments\n", ident, "", (int)a->arguments.size());
        for (const auto & arg : a->arguments) printAST(arg, ident + 3);
        if (a->return_type) {
            printf("%*s and return type:\n", ident, "");
            printAST(a->return_type, ident + 3);
        } else {
            printf("%*s  and no return type, void inferred\n", ident, "");
        }
        break;
    }
    case AST_STATEMENT_BLOCK: {
        const StatementBlockAST* a = (const StatementBlockAST *)ast;
        printf("%*sStatementBlockAST with %d statements\n", ident, "", (int)a->statements.size());
        for (const auto & stmt : a->statements) printAST(stmt, ident + 3);
        break;
    }
    case AST_RETURN_STATEMENT: {
        const ReturnStatementAST *a = (const ReturnStatementAST *)ast;
        printf("%*sReturnStatementAST\n", ident, "");
        printAST(a->ret, ident + 3);
        break;
    }
    case AST_FUNCTION_DEFINITION: {
        const FunctionDefinitionAST *a = (const FunctionDefinitionAST *)ast;
        printf("%*sFunctionDefinitionAST\n", ident, "");
        printAST(a->declaration, ident + 3);
        printAST(a->function_body, ident + 3);
        break;
    }
    case AST_IDENTIFIER: {
        const IdentifierAST *a = (const IdentifierAST *)ast;
        printf("%*sIdentifierAST name: [%s]\n", ident, "", a->name);
        break;
    }
    case AST_FUNCTION_CALL: {
        const FunctionCallAST *a = (const FunctionCallAST *)ast;
        printf("%*sFunctionCall: %s with %d arguments\n", ident, "", a->function_name, (int)a->args.size());
        for (auto arg : a->args) printAST(arg, ident + 3);
        break;
    }
    case AST_FILE: {
        const FileAST *a = (const FileAST *)ast;
        for (const auto &it : a->items) printAST(it, ident);
        break;
    }
    default : 
        printf("%*sUnknown AST type\n", ident, "");
    }
}

