#include "c_generator.h"
#include <stdio.h>
#include <stdlib.h>

static inline bool isFunctionDeclaration(VariableDeclarationAST *decl)
{
    return (decl->specified_type->ast_type == AST_FUNCTION_TYPE);
}

static inline bool isStringDeclaration(VariableDeclarationAST *decl)
{
    if (decl->specified_type->ast_type == AST_DIRECT_TYPE) {
        auto dt = (DirectTypeAST *)decl->specified_type;
        return dt->type == BASIC_TYPE_STRING;
    }
    return false;
}

void c_generator::generate_preamble()
{
    fprintf(output_file, "#include <stdio.h>\n");
    fprintf(output_file, "#include <stdlib.h>\n\n");
    fprintf(output_file, "typedef signed char         s8;\n");
    fprintf(output_file, "typedef signed short       s16;\n");
    fprintf(output_file, "typedef int                s32;\n");
    fprintf(output_file, "typedef long long          s64;\n");
    fprintf(output_file, "typedef unsigned char       u8;\n");
    fprintf(output_file, "typedef unsigned short     u16;\n");
    fprintf(output_file, "typedef unsigned int       u32;\n");
    fprintf(output_file, "typedef unsigned long long u64;\n");
    fprintf(output_file, "typedef float              f32;\n");
    fprintf(output_file, "typedef double             f64;\n\n");
    fprintf(output_file, "struct string {\n");
    fprintf(output_file, "    char *data;\n");
    fprintf(output_file, "    u32 size;\n");
    fprintf(output_file, "};\n");
    fprintf(output_file, "\n");
}

void c_generator::do_ident()
{
    if (ident > 0) fprintf(output_file, "%*s", ident, "");
}

void c_generator::generate_function_prototype(VariableDeclarationAST * decl)
{
    // no ident since prototypes are always top level

    assert(decl->specified_type->ast_type == AST_FUNCTION_TYPE);

    auto ft = (FunctionTypeAST *)decl->specified_type;
    // first print the return type
    if (ft->return_type) {
        generate_type(ft->return_type);
    } else {
        fprintf(output_file, "void");
    }
    if (decl->flags & DECL_FLAG_IS_CONSTANT) {
        fprintf(output_file, " %s", decl->varname);
    } else if (decl->definition) {
        // if we have a definition, we need to write it somewhere
        fprintf(output_file, " %s_implementation ", decl->varname);
    }
    fprintf(output_file, "(");
    // now print the argument declarations here
    bool first = true;
    for (auto arg : ft->arguments) {
        if (!first) fprintf(output_file, ", ");
        generate_argument_declaration(arg);
        first = false;
    }
    fprintf(output_file, ");\n");
}

void c_generator::generate_variable_declaration(VariableDeclarationAST * decl)
{
    do_ident();

    if (decl->specified_type->ast_type == AST_DIRECT_TYPE) {
        auto dt = (DirectTypeAST *)decl->specified_type;
        fprintf(output_file, BasicTypeToStr(dt->type));
        fprintf(output_file, " %s", decl->varname);

        if (decl->definition) {
            fprintf(output_file, " = ");

            // special use for strings since they are a struct
            if (isStringDeclaration(decl)) {
                fprintf(output_file, "{ ");
            }
            generate_expression((ExpressionAST *)decl->definition);
            if (isStringDeclaration(decl)) {
                fprintf(output_file, ", 0 }");
            }
        }
        fprintf(output_file, ";\n");
    } else if (decl->specified_type->ast_type == AST_FUNCTION_TYPE) {
        auto ft = (FunctionTypeAST *)decl->specified_type;
        // first print the return type
        if (ft->return_type) {
            generate_type(ft->return_type);
        } else {
            fprintf(output_file, "void");
        }
        if (decl->flags & DECL_FLAG_IS_CONSTANT) {
            fprintf(output_file, " %s ", decl->varname);
        } else {
            fprintf(output_file, " (*%s) ", decl->varname);
        }
        fprintf(output_file, "(");
        // now print the argument declarations here
        bool first = true;
        for (auto arg : ft->arguments) {
            if (!first) fprintf(output_file, ", ");
            generate_argument_declaration(arg);
            first = false;
        }
        fprintf(output_file, ")");

        // now make a decision to end the declaration (prototype, function pointer) 
        // or to make this the full function definition
        if (decl->flags & DECL_FLAG_IS_CONSTANT) {
            fprintf(output_file, "\n");
            // if the function is constant, we can declare it here
            // @TODO: think about prototypes
            assert(decl->definition);
            assert(decl->definition->ast_type == AST_FUNCTION_DEFINITION);
            auto fundef = (FunctionDefinitionAST *)decl->definition;
            generate_statement_block(fundef->function_body);
            fprintf(output_file, "\n");
        } else {
            // even if the function is not constant, for C uses we might need
            // to create an implementation and assign it
            fprintf(output_file, ";\n");
        }
    } else {
        assert(!"Type not suported on C code generation yet");
    }

}

void c_generator::generate_argument_declaration(ArgumentDeclarationAST * arg)
{
    assert(arg->type->ast_type == AST_DIRECT_TYPE);
    auto dt = (DirectTypeAST *)arg->type;
    fprintf(output_file, BasicTypeToStr(dt->type));
    fprintf(output_file, " %s", arg->name);
}

void c_generator::generate_statement_block(StatementBlockAST * block)
{
    do_ident();
    fprintf(output_file, "{\n");
    ident += 4;

    for (auto stmt : block->statements) generate_statement(stmt);

    ident -= 4;
    do_ident();
    fprintf(output_file, "}\n");
}

void c_generator::generate_statement(StatementAST * stmt)
{
    switch (stmt->ast_type) {
    case AST_VARIABLE_DECLARATION: {
        generate_variable_declaration((VariableDeclarationAST *)stmt);
        break;
    }
    case AST_RETURN_STATEMENT: {
        generate_return_statement((ReturnStatementAST *)stmt);
        break;
    }
    case AST_ASSIGNMENT: {
        do_ident();
        generate_assignment((AssignmentAST *)stmt);
        fprintf(output_file, ";\n");
        break;
    }
    case AST_FUNCTION_CALL: {
        do_ident();
        generate_function_call((FunctionCallAST *)stmt);
        fprintf(output_file, ";\n");
        break;
    }
    default:
        assert(!"Not implemented yet");
    }
}

void c_generator::generate_return_statement(ReturnStatementAST * ret)
{
    do_ident();
    fprintf(output_file, "return ");
    generate_expression(ret->ret);
    fprintf(output_file, ";\n");
}

void c_generator::generate_assignment(AssignmentAST * assign)
{
    generate_expression(assign->lhs);

    fprintf(output_file, TokenTypeToCOP(assign->op));

    generate_expression(assign->rhs);
}

void c_generator::generate_expression(ExpressionAST * expr)
{
    switch (expr->ast_type) {
    case AST_CONSTANT_NUMBER: {
        auto cn = (ConstantNumberAST *)expr;
        if ((cn->type == BASIC_TYPE_F32) ||
            (cn->type == BASIC_TYPE_F64)) {
            fprintf(output_file, "%f", cn->pl.pf64);
        } else if (cn->type == BASIC_TYPE_U64) {
            fprintf(output_file, "%lld", cn->pl.pu64);
        }
        break;
    }
    case AST_CONSTANT_STRING: {
        auto cs = (ConstantStringAST *)expr;
        fprintf(output_file, "\"%s\"", cs->str);
        break;
    }
    case AST_IDENTIFIER: {
        auto iden = (IdentifierAST *)expr;
        fprintf(output_file, "%s", iden->name);
        break;
    }
    case AST_BINARY_OPERATION: {
        auto binop = (BinaryOperationAST *)expr;
        generate_expression(binop->lhs);

        fprintf(output_file, TokenTypeToCOP(binop->op));

        generate_expression(binop->rhs);
        break;
    }
    case AST_FUNCTION_CALL: {
        generate_function_call((FunctionCallAST *)expr);
        break;
    }
    default:
        assert(!"We should never get here");
    }
}

void c_generator::generate_function_call(FunctionCallAST * call)
{
    fprintf(output_file, "%s(", call->function_name);
    bool first = true;
    for (auto arg : call->args) {
        if (!first) fprintf(output_file, ", ");
        generate_expression(arg);
        first = false;
    }
    fprintf(output_file, ")");
}

void c_generator::generate_type(BaseAST * ast)
{
    assert(ast->ast_type == AST_DIRECT_TYPE);
    auto dt = (DirectTypeAST *)ast;
    fprintf(output_file, BasicTypeToStr(dt->type));
}


void c_generator::generate_c_file(const char * filename, FileAST * root)
{
    output_file = fopen(filename, "w");
    ident = 0;
    generate_preamble();

    for (auto &ast : root->items) {
        if (ast->ast_type == AST_VARIABLE_DECLARATION) {
            auto decl = (VariableDeclarationAST *)ast;
            if (decl->specified_type->ast_type == AST_FUNCTION_TYPE) {
                generate_function_prototype(decl);
            }
        }            
    };
    fprintf(output_file, "\n\n");

    for (auto &ast : root->items) {
        switch (ast->ast_type) {
        case AST_VARIABLE_DECLARATION: {
            generate_variable_declaration(ast);
            break;
        }
        default:
            printf("AST type not supported at the top level: %d\n", ast->ast_type);
        }
    }

    fclose(output_file);
    output_file = nullptr;
}
