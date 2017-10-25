#include "AST.h"

static const char *BasicTypeToStr(BasicType t)
{
    switch (t)
    {
    case I8:  return "I8";
    case I16: return "I16";
    case I32: return "I32";
    case I64: return "I64";
    case U8:  return "U8";
    case U16: return "U16";
    case U32: return "U32";
    case U64: return "U64";
    case F32: return "F32";
    case F64: return "F64";
    }
    return "UNKNOWN";
}

void ConstNumAST::print(int ident)
{
    printf("%*sConstNumAST type: %s", ident, "", BasicTypeToStr(type));
    switch (type)
    {
    case F32: 
        printf(" %f", pl.pf32);
        break;
    case F64:
        printf(" %lf", pl.pf64);
        break;
    case U32:
        printf(" %d", pl.pu32);
        break;
    case U64:
        printf(" %lld", pl.pu64);
        break;
    }
    printf("\n");
}

void BinOpAST::print(int ident)
{
    printf("%*sBinOpAST op: %s\n", ident, "", TokenTypeToStr(op));
    printf("%*s LHS:\n", ident, "");
    lhs->print(ident + 3);
    printf("%*s RHS:\n", ident, "");
    rhs->print(ident + 3);
}

void UnOpAST::print()
{
}

void AssignAST::print(int ident)
{
    printf("%*sAssignAST op: %s\n", ident, "", TokenTypeToStr(op));
    printf("%*s LHS:\n", ident, "");
    lhs->print(ident + 3);
    printf("%*s RHS:\n", ident, "");
    rhs->print(ident + 3);
}

void DeclAST::print(int ident)
{
    printf("%*sDeclAST varname: [%s] is_constant: %s\n", ident, "", varname,
        (is_constant ? "YES" : "NO"));
    printf("%*s SpecifiedType: ", ident, "");
    if (specified_type) {
        printf("\n");
        specified_type->print(ident + 3);
    } else {
        printf(" NONE\n");
    }
    printf("%*s InferredType: ", ident, "");
    if (inferred_type) {
        printf("\n");
        inferred_type->print(ident + 3);
    } else {
        printf(" NONE\n");
    }
    printf("%*s DefinitionAST: ", ident, "");
    if (definition) {
        printf("\n");
        definition->print(ident + 3);
    } else {
        printf(" NONE\n");
    }
}

void DirectTypeAST::print(int ident)
{
    printf("%*sDirectTypeAST name: [%s]", ident, "", name);
    if (isString) {
        printf(" type: STRING\n");
    } else {
        printf(" type: %s\n", BasicTypeToStr(type));
    }
}

void ArgumentDeclarationAST::print(int ident)
{
    printf("%*sArgumentDeclarationAST name: %s\n", ident, "", name);
    if (type) type->print(ident + 3);
}

void FunctionDeclarationAST::print(int ident)
{
    printf("%*sFunctionDeclarationAST with %d arguments\n", ident, "", (int)arguments.size());
    for (auto arg : arguments) arg->print(ident + 3);
    if (return_type) {
        printf(" and return type:\n");
        return_type->print(ident + 3);
    } else {
        printf(" and no return type, void inferred\n");
    }
}

void StatementBlockAST::print(int ident)
{
    printf("%*sStatementBlockAST with %d statements\n", ident, "", (int)statements.size());
    for (auto stmt : statements) stmt->print(ident + 3);
}

void ReturnStatementAST::print(int ident)
{
    printf("%*sReturnStatementAST\n", ident, "");
    ret->print(ident + 3);
}

void FunctionDefinitionAST::print(int ident)
{
    printf("%*sFunctionDefinitionAST\n", ident, "");
    declaration->print(ident + 3);
    function_body->print(ident + 3);
}

void IdentAST::print(int ident)
{
    printf("%*sIdentAST name: [%s]\n", ident, "", name);
}

void ConstStringAST::print(int ident)
{
    printf("%*sConstStringAST name: [%s]\n", ident, "", str);
}
