#include "AST.h"
#include <stdio.h>

const char *BasicTypeToStr(const DirectTypeAST *t)
{
    switch (t->basic_type)
    {
    case BASIC_TYPE_VOID: return "void";
    case BASIC_TYPE_BOOL:  return "bool";
    case BASIC_TYPE_STRING:  return "string";
    case BASIC_TYPE_CUSTOM: return t->name;
    case BASIC_TYPE_INTEGER: {
        if (t->isSigned) {
            switch (t->size_in_bytes) {
            case 1: return "s8";
            case 2: return "s16";
            case 4: return "s32";
            default:
            case 8: return "s64";
            }
            assert(false);
            return "UNKNOWN";
        } else {
            switch (t->size_in_bytes) {
            case 1: return "u8";
            case 2: return "u16";
            case 4: return "u32";
            default:
            case 8: return "u64";
            }
            assert(false);
            return "UNKNOWN";
        }
    }
    case BASIC_TYPE_FLOATING: 
        if (t->size_in_bytes == 4) return "f32";
        if (t->size_in_bytes == 8) return "f64";
        assert(false);
        return "UNKNOWN";
    }
    return "UNKNOWN";
}

static const char *Array_Type_ToStr(ArrayTypeAST::Array_Type at)
{
    if (at == ArrayTypeAST::UNKNOWN_ARRAY) {
        return "UNKNOWN ARRAY";
    } else if (at == ArrayTypeAST::STATIC_ARRAY) {
        return "STATIC_ARRAY";
    } else if (at == ArrayTypeAST::SIZED_ARRAY) {
        return "SIZED_ARRAY";
    } else if (at == ArrayTypeAST::DYNAMIC_ARRAY) {
        return "DYNAMIC_ARRAY";
    } else {
        return "INVALID ARRAY TYPE";
    }
}

static const char *BoolToStr(bool b)
{
    if (b) return "YES";
    return "NO";
}

static void printDeclarationASTFlags(u32 flags)
{
    if (flags & DECL_FLAG_HAS_BEEN_C_GENERATED) {
        printf("DECL_FLAG_HAS_BEEN_GENERATED ");
    }
    if (flags & DECL_FLAG_HAS_BEEN_INFERRED) {
        printf("DECL_FLAG_HAS_BEEN_INFERRED ");
    }
    if (flags & DECL_FLAG_IS_CONSTANT) {
        printf("DECL_FLAG_IS_CONSTANT ");
    }
}

extern bool option_printSeq;

void printAST(const BaseAST *ast, int ident)
{
    if (ast == nullptr) return;

    u32 ex = 0;
    if (option_printSeq) {
        printf("%04u ", (u32)ast->s);
        ex += 5;
    }

    switch (ast->ast_type) {
    case AST_LITERAL: {
        const LiteralAST *c = (const LiteralAST *)ast;
        printf("%*sLiteralAST type: %s", ident, "", 
            BasicTypeToStr(c->typeAST));
        switch (c->typeAST->basic_type)
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
            if (c->typeAST->isSigned) {
                printf(" %" U64FMT "d", c->_s64);
            } else {
                printf(" %" U64FMT "u", c->_u64);
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
        printf("%*s LHS:\n", ident + ex, "");
        printAST(b->lhs, ident + 3);
        printf("%*s RHS:\n", ident + ex, "");
        printAST(b->rhs, ident + 3);
        break;
    }
    case AST_ASSIGNMENT: {
        const AssignmentAST *a = (const AssignmentAST *)ast;
        printf("%*sAssignAST op: %s\n", ident, "", TokenTypeToStr(a->op));
        printf("%*s LHS:\n", ident + ex, "");
        printAST(a->lhs, ident + 3);
        printf("%*s RHS:\n", ident + ex, "");
        printAST(a->rhs, ident + 3);
        break;
    }
    case AST_VARIABLE_DECLARATION: {
        const VariableDeclarationAST *a = (const VariableDeclarationAST *)ast;
        printf("%*sDeclAST varname: [%s] flags: ", ident, "", a->varname);
        printDeclarationASTFlags(a->flags);     
        printf("\n%*s SpecifiedType: ", ident+ ex, "");
        if (a->specified_type) {
            printf("\n");
            printAST(a->specified_type, ident + 3);
        } else {
            printf(" NONE\n");
        }
        printf("%*s DefinitionAST: ", ident + ex, "");
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
    case AST_POINTER_TYPE: {
        auto pt = (const PointerTypeAST *)ast;
        printf("%*sPointerTypeAST\n", ident, "");
        printAST(pt->points_to_type, ident + 3);
        break;
    }
    case AST_FUNCTION_TYPE: {
        const FunctionTypeAST *a = (const FunctionTypeAST *)ast;
        printf("%*sFunctionDeclarationAST with %d arguments\n", ident, "", (int)a->arguments.size());
        printf("%*s  Foreign: %s VariableArguments: %s\n", ident + ex, "", BoolToStr(a->isForeign), BoolToStr(a->hasVariableArguments));
        for (const auto & arg : a->arguments) printAST(arg, ident + 3);
        if (a->return_type) {
            printf("%*s and return type:\n", ident + ex, "");
            printAST(a->return_type, ident + 3);
        } else {
            printf("%*s  and no return type, void inferred\n", ident + ex, "");
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
    case AST_IF_STATEMENT: {
        const IfStatementAST *a = (const IfStatementAST *)ast;
        printf("%*sIfStatementAST\n", ident, "");
        printAST(a->condition, ident + 3);
        printAST(a->then_branch, ident + 3);
        if (a->else_branch) printAST(a->else_branch, ident + 3);
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
        printAST(a->next, ident + 3);
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
    case AST_UNARY_OPERATION: {
        auto un = (const UnaryOperationAST *)ast;
        printf("%*sUnaryOperation: op: %s\n", ident, "", TokenTypeToStr(un->op));
        printAST(un->expr, ident + 3);
        break;
    }
    case AST_STRUCT_ACCESS: {
        auto sac = (const StructAccessAST *)ast;
        printf("%*sStruct Access: name: %s\n", ident, "", sac->name);
        printAST(sac->next, ident + 3);
        break;
    }
    case AST_ARRAY_ACCESS: {
        auto acc = (const ArrayAccessAST *)ast;
        printf("%*sArrayAccess:\n", ident, "");
        printAST(acc->array_exp, ident + 3);
        printAST(acc->access_type, ident + 3);
        printAST(acc->next, ident + 6);
        break;
    }
    case AST_STRUCT_DEFINITION: {
        auto struct_def = (const StructDefinitionAST *)ast;
        printf("%*sStructDefinition: StructType: \n", ident, "");
        for (auto decl : struct_def->struct_type->struct_scope.decls) {
            printAST(decl, ident + 3);
        }
        break;
    }
    case AST_STRUCT_TYPE: {
        auto struct_type = (const StructTypeAST *)ast;
        printf("%*sStructType: \n", ident, "");
        for (auto decl : struct_type->struct_scope.decls) {
            printAST(decl, ident + 3);
        }
        break;
    }
    case AST_ARRAY_TYPE: {
        auto atype = (const ArrayTypeAST *)ast;
        printf("%*sArrayType: \n", ident, "");
        printf("%*sArray of Type: \n", ident+3+ex, "");
        printAST(atype->array_of_type, ident + 3);
        printf("%*sNumElems: %" U64FMT "u\n", ident + 3+ex, "", atype->num_elems);
        printf("%*sNumber Expression: \n", ident+3+ex, "");
        printAST(atype->num_expr, ident + 3);
        printf("%*sArray Type: %s  Flags: %x\n", ident + 3+ex, "", 
            Array_Type_ToStr(atype->array_type), atype->flags);
        break;
    }
    case AST_RUN_DIRECTIVE: {
        auto run = (const RunDirectiveAST *)ast;
        printf("%*sRun Directive:\n", ident, "");
        printAST(run->expr, ident + 3);
        break;
    }
    default : 
        printf("%*sUnknown AST type\n", ident, "");
        assert(false);
    }
}

DirectTypeAST *findFinalDirectType(PointerTypeAST *pt)
{
    assert(pt->points_to_type);
    switch (pt->points_to_type->ast_type) {
    case AST_POINTER_TYPE:
        return findFinalDirectType((PointerTypeAST *)pt->points_to_type);
    case AST_DIRECT_TYPE:
        return (DirectTypeAST *)pt->points_to_type;
    case AST_ARRAY_TYPE:
        return findFinalDirectType((ArrayTypeAST *)pt->points_to_type);
    default:
        assert(!"A pointer should not point to anything else");
        return nullptr;
    }
}

DirectTypeAST *findFinalDirectType(ArrayTypeAST *at)
{
    assert(at->array_of_type);
    switch (at->array_of_type->ast_type) {
    case AST_POINTER_TYPE:
        return findFinalDirectType((PointerTypeAST *)at->array_of_type);
    case AST_DIRECT_TYPE:
        return (DirectTypeAST *)at->array_of_type;
    case AST_ARRAY_TYPE:
        return findFinalDirectType((ArrayTypeAST *)at->array_of_type);
    default:
        assert(!"A pointer should not point to anything else");
        return nullptr;
    }
}
