#include "c_generator.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef WIN32
# define sprintf_s sprintf
#endif

static inline bool isStringDeclaration(VariableDeclarationAST *decl)
{
    if (decl->specified_type->ast_type == AST_DIRECT_TYPE) {
        auto dt = (DirectTypeAST *)decl->specified_type;
        return dt->basic_type == BASIC_TYPE_STRING;
    }
    return false;
}

static bool isStringDefinition(DefinitionAST *def)
{
    if (def->ast_type == AST_LITERAL) {
        auto lit = (LiteralAST *)def;
        return lit->typeAST.basic_type == BASIC_TYPE_STRING;
    }
    return false;
}

static const char *boolToStr(bool b)
{
    if (b) return "true";
    return "false";
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

void c_generator::generate_line_info(BaseAST * ast)
{
    if ((last_filename != ast->filename) || (last_linenum != ast->line_num)) {
        last_linenum = ast->line_num;
        last_filename = ast->filename;
        fprintf(output_file, "#line %d \"%s\"\n", ast->line_num, ast->filename);
    }
}

void c_generator::generate_dangling_functions()
{
    for (auto decl : dangling_functions) {
        do_ident();
        fprintf(output_file, "%s = %s_implementation;\n", decl->varname, decl->varname);
    }
}

void c_generator::generate_function_prototype(VariableDeclarationAST * decl, bool second_pass)
{
    // no ident since prototypes are always top level

    assert(decl->specified_type->ast_type == AST_FUNCTION_TYPE);

    auto ft = (FunctionTypeAST *)decl->specified_type;

    bool isMain = !strcmp(decl->varname, "main");
    
    // first print the return type
    if (ft->return_type) {
        generate_type(ft->return_type);
    } else {
        if (isMain) {
            // main is a special case, to make the C compiler happy, allow void to be int
            fprintf(output_file, "int");
        } else {
            fprintf(output_file, "void");           
        }
    }
    if (decl->flags & DECL_FLAG_IS_CONSTANT) {
        fprintf(output_file, " %s ", decl->varname);
    } else if (decl->definition) {
        if (!second_pass && decl->definition->ast_type == AST_FUNCTION_DEFINITION) {

            /* if we have a definition, we need to write it somewhere
               only do this if the definition is a function body, not 
               for example a variable

               The purpose of the second pass variable is to ensure that function pointers
               do get a prototype in the prototype section

            */
            fprintf(output_file, " %s_implementation ", decl->varname);
            dangling_functions.push_back(decl);

        } else {
            fprintf(output_file, " (*%s) ", decl->varname);
        }
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
    fprintf(output_file, ");\n");

    if (!second_pass && decl->definition && decl->definition->ast_type == AST_FUNCTION_DEFINITION &&
        !(decl->flags & DECL_FLAG_IS_CONSTANT) && !isMain) 
        generate_function_prototype(decl, true);
    
}

void c_generator::generate_struct_prototype(VariableDeclarationAST * decl)
{
 
    auto stype = (StructTypeAST *)decl->specified_type;

    fprintf(output_file, "struct %s;\n", decl->varname);
}

void c_generator::ensure_deps_are_generated(StructTypeAST *stype)
{
    for (auto mem : stype->struct_scope.decls) {
        if (mem->specified_type->ast_type == AST_DIRECT_TYPE) {
            auto dt = (DirectTypeAST *)mem->specified_type;
            if (dt->custom_type && (dt->custom_type->ast_type == AST_STRUCT_TYPE)) {
                auto st = (StructTypeAST *)dt->custom_type;
                if (st->decl && !(st->decl->flags & DECL_FLAG_HAS_BEEN_GENERATED)) {
                    generate_variable_declaration(st->decl);
                }
            }
        }
    }
}


void c_generator::generate_variable_declaration(VariableDeclarationAST * decl)
{
    // If this is a struct, ensure dependent types are declared first
    if (decl->specified_type->ast_type == AST_STRUCT_TYPE) {        
        auto stype = (StructTypeAST *)decl->specified_type;
        ensure_deps_are_generated(stype);
    }

    if (decl->flags & DECL_FLAG_HAS_BEEN_GENERATED) {
        // if we generated this already, nothing to do
        return;
    }

    generate_line_info(decl);
    do_ident();

    if (decl->specified_type->ast_type == AST_DIRECT_TYPE) {
        auto dt = (DirectTypeAST *)decl->specified_type;
        if (dt->basic_type == BASIC_TYPE_CUSTOM) {
            fprintf(output_file, "%s", dt->name);
        } else {
            fprintf(output_file, BasicTypeToStr(dt));
        }
        fprintf(output_file, " %s", decl->varname);
        
        if (decl->definition) {
            fprintf(output_file, " = ");

            if (isStringDefinition(decl->definition)) {
                // special use for strings since they are a struct
                if (isStringDeclaration(decl)) {
                    fprintf(output_file, "{ ");
                }
                generate_expression((ExpressionAST *)decl->definition);
                if (isStringDeclaration(decl)) {
                    fprintf(output_file, ", 0 }");
                }
            } else {
                generate_expression((ExpressionAST *)decl->definition);
            }
        }
        fprintf(output_file, ";\n");
    } else if (decl->specified_type->ast_type == AST_FUNCTION_TYPE) {
        auto ft = (FunctionTypeAST *)decl->specified_type;

        bool isMain = !strcmp(decl->varname, "main");

        // foreign functions are prototype only
        if (ft->isForeign) return;

        // if this is a function ptr in C, it's been defined already in
        // the prototypes section, skip it here
        if (!(decl->flags & DECL_FLAG_IS_CONSTANT)) {
            if (decl->definition && decl->definition->ast_type != AST_FUNCTION_DEFINITION) {
                return;
            }
            if (!decl->definition) {
                // a pure function pointer might not have a definition at all
                return;
            }
            if (decl->definition && decl->definition->ast_type == AST_FUNCTION_DEFINITION) {
                // let's write here the actual implementation, now. 
                // and later we assign it. 
                VariableDeclarationAST impl;
                char func_name[256];
                sprintf_s(func_name, "%s_implementation", decl->varname);
                memcpy(&impl, decl, sizeof(impl));
                impl.flags ^= DECL_FLAG_IS_CONSTANT;
                impl.varname = func_name;
                generate_variable_declaration(&impl);
                return;
            }

        }

        // first print the return type
        if (ft->return_type) {
            generate_type(ft->return_type);
        } else {
            if (isMain) {
                // main is a special case, to make the C compiler happy, allow void to be int
                fprintf(output_file, "int");
            } else {
                fprintf(output_file, "void");
            }
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
            if (!strcmp(decl->varname, "main")) {
                insert_dangling_funcs = true;
            }
            auto fundef = (FunctionDefinitionAST *)decl->definition;
            generate_statement_block(fundef->function_body);
            fprintf(output_file, "\n");
        } else {
            // even if the function is not constant, for C uses we might need
            // to create an implementation and assign it
            fprintf(output_file, ";\n");

        }
    } else if (decl->specified_type->ast_type == AST_STRUCT_TYPE) {
        auto stype = (StructTypeAST *)decl->specified_type;
        fprintf(output_file, "struct %s {\n", decl->varname);
        ident += 4;
        for (auto mem : stype->struct_scope.decls) {
            generate_variable_declaration(mem);
        }
        ident -= 4;
        do_ident();
        fprintf(output_file, "};\n");
    } else {
        assert(!"Type not suported on C code generation yet");
    }

    decl->flags |= DECL_FLAG_HAS_BEEN_GENERATED;
}

void c_generator::generate_argument_declaration(VariableDeclarationAST * arg)
{
    assert(arg->specified_type->ast_type == AST_DIRECT_TYPE);
    auto dt = (DirectTypeAST *)arg->specified_type;
    fprintf(output_file, BasicTypeToStr(dt));
    fprintf(output_file, " %s", arg->varname);
}

void c_generator::generate_statement_block(StatementBlockAST * block)
{
    generate_line_info(block);
    do_ident();
    fprintf(output_file, "{\n");
    ident += 4;

    if (insert_dangling_funcs) {
        generate_dangling_functions();
        insert_dangling_funcs = false;
    }

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
        generate_line_info(stmt);
        do_ident();
        generate_assignment((AssignmentAST *)stmt);
        fprintf(output_file, ";\n");
        break;
    }
    case AST_FUNCTION_CALL: {
        generate_line_info(stmt);
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
    generate_line_info(ret);
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
    case AST_LITERAL: {
        auto lit = (LiteralAST *)expr;
        switch (lit->typeAST.basic_type) {
        case BASIC_TYPE_FLOATING:
            fprintf(output_file, "%f", lit->_f64);
            break;
        case BASIC_TYPE_BOOL:
            fprintf(output_file, "%s", boolToStr(lit->_bool));
            break;
        case BASIC_TYPE_STRING:
            fprintf(output_file, "\"%s\"", lit->str);
            break;
        case BASIC_TYPE_INTEGER:
            if (lit->typeAST.isSigned) fprintf(output_file, "%" U64FMT "d", lit->_s64);
            else fprintf(output_file, "%" U64FMT "u", lit->_u64);
            break;
        }

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
    case AST_UNARY_OPERATION: {
        auto unop = (UnaryOperationAST *)expr;
        fprintf(output_file, TokenTypeToCOP(unop->op));

        generate_expression(unop->expr);
        break;
    }
    case AST_RUN_DIRECTIVE: {
        // we should never get here, do something crappy for now
        fprintf(output_file, "0");
        break;
    } 
    case AST_VAR_REFERENCE: {
        auto vref = (VarReferenceAST *)expr;

        fprintf(output_file, "%s.", vref->name);
        generate_expression(vref->next);
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
    fprintf(output_file, BasicTypeToStr(dt));
}


void c_generator::generate_c_file(const char * filename, FileAST * root)
{
#ifdef WIN32
    fopen_s(&output_file, filename, "w");
#else
    output_file = fopen(filename, "w");
#endif      
    ident = 0;
    last_filename = nullptr;
    last_linenum = 0;
    dangling_functions.reset();
    insert_dangling_funcs = false;
    // dangling functions have an issue with possible local functions
    // also named main... but it is remote

    generate_preamble();

    // when we have them, place custom types here, before prototypes
    // maybe type prototypes (forward decls)

    // write function prototypes
    for (auto &ast : root->items) {
        if (ast->ast_type == AST_VARIABLE_DECLARATION) {
            auto decl = (VariableDeclarationAST *)ast;
            if (decl->specified_type->ast_type == AST_FUNCTION_TYPE) {
                generate_function_prototype(decl);
            } else if (decl->specified_type->ast_type == AST_STRUCT_TYPE) {
                generate_struct_prototype(decl);
            }
        }            
    };
    fprintf(output_file, "\n\n");

    for (auto &ast : root->items) {
        switch (ast->ast_type) {
        case AST_VARIABLE_DECLARATION: {
            generate_variable_declaration((VariableDeclarationAST *)ast);
            break;
        }
        default:
            printf("  C Generation: AST type not supported at the top level: %d\n", ast->ast_type);
        }
    }

    fclose(output_file);
    output_file = nullptr;
}
