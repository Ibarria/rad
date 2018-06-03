#pragma once

#include "Array.h"
#include "mytypes.h"
#include "TokenType.h"
#include "TextType.h"
#include "Profiler.h"
#include "Hash.h"

struct BaseAST;
struct TypeAST;
struct ExpressionAST;
struct VariableDeclarationAST;
struct FunctionDefinitionAST;
struct RunDirectiveAST;
struct bytecode_function;
struct interp_deps;

namespace llvm { class Value; class Type; }

struct Scope {
    Scope *parent = nullptr;
    Array<VariableDeclarationAST *>decls;
    Array<TypeAST *>def_types;
    FunctionDefinitionAST *current_function = nullptr;
};

enum BasicType {
    BASIC_TYPE_VOID,
    BASIC_TYPE_BOOL,
    BASIC_TYPE_STRING,
    BASIC_TYPE_INTEGER,
    BASIC_TYPE_FLOATING,
    BASIC_TYPE_CUSTOM
};

enum AST_CLASS_TYPE {
    AST_UNKNOWN,
    AST_FILE,
    AST_FUNCTION_TYPE,
    AST_STATEMENT_BLOCK,
    AST_RETURN_STATEMENT,
    AST_IF_STATEMENT,
    AST_FUNCTION_DEFINITION,
    AST_FUNCTION_CALL,
    AST_DIRECT_TYPE,
    AST_POINTER_TYPE,
    AST_ARRAY_TYPE,
    AST_IDENTIFIER,
    AST_LITERAL,
    AST_BINARY_OPERATION,
    AST_UNARY_OPERATION,
    AST_ASSIGNMENT,
    AST_VARIABLE_DECLARATION,
    AST_RUN_DIRECTIVE,
    AST_STRUCT_TYPE,
    AST_STRUCT_DEFINITION,   // this is here only to satisfy class hierarchy... does not add value    
    AST_ARRAY_ACCESS,
    AST_STRUCT_ACCESS,
};

struct BaseAST
{
    AST_CLASS_TYPE ast_type;
    BaseAST() { ast_type = AST_UNKNOWN; }
    TextType filename = nullptr;
    Scope *scope = nullptr;
    u32 line_num = 0;
    u32 char_num = 0;
	u64 s = 0; // This is a unique ID for this AST element
};

struct FileAST : BaseAST
{
    FileAST() { ast_type = AST_FILE; }
    Array<BaseAST *>items;
    Array<RunDirectiveAST *>run_items;
    Scope global_scope;
    ImportsHash imports;
};

struct StatementAST : BaseAST
{
    llvm::Value *codegen = nullptr;
};

struct DefinitionAST : StatementAST
{
    bool needsSemiColon = true;
};

struct TypeAST : BaseAST
{
    u32 size_in_bytes = 0;
    llvm::Type *llvm_type = nullptr;
};

struct FunctionTypeAST : TypeAST
{
    FunctionTypeAST() { ast_type = AST_FUNCTION_TYPE; size_in_bytes = 8; }
    Array<VariableDeclarationAST *> arguments;
    TypeAST *return_type = nullptr;
    void *func_ptr = nullptr; // This will hold the address for dyncall
    bool isForeign = false;
    bool hasVariableArguments = false;
};

struct StatementBlockAST : StatementAST
{
    StatementBlockAST() { ast_type = AST_STATEMENT_BLOCK; }
    Array<StatementAST *> statements;
    Scope block_scope;
    llvm::Value *codegen = nullptr;
};

struct ReturnStatementAST: StatementAST
{
    ReturnStatementAST() { ast_type = AST_RETURN_STATEMENT; }
    ExpressionAST *ret = nullptr;
    llvm::Value *codegen = nullptr;
};

struct IfStatementAST : StatementAST
{
    IfStatementAST() { ast_type = AST_IF_STATEMENT; }
    ExpressionAST *condition = nullptr;
    StatementAST *then_branch = nullptr;
    StatementAST *else_branch = nullptr;
    llvm::Value *codegen = nullptr;
};

struct FunctionDefinitionAST : DefinitionAST
{
    FunctionDefinitionAST() { ast_type = AST_FUNCTION_DEFINITION; needsSemiColon = false; }
    FunctionTypeAST *declaration = nullptr;
    StatementBlockAST *function_body = nullptr;
    VariableDeclarationAST *var_decl = nullptr; // so that we can find the name of the function
    u32 size_in_bytes = 8;
    bytecode_function *bc_function = nullptr;
    bool being_generated = false;
};

struct ExpressionAST : DefinitionAST
{
    TypeAST *expr_type = nullptr;
};

struct RunDirectiveAST : ExpressionAST
{
    RunDirectiveAST() { ast_type = AST_RUN_DIRECTIVE; }
    BaseAST *new_ast = nullptr;
    ExpressionAST *expr = nullptr;
	s16 reg = -1;
    interp_deps *deps = nullptr; // only applicable for global directives
    bytecode_function *bc_function = nullptr;
    bool being_generated = false;
};

struct FunctionCallAST : ExpressionAST
{
    FunctionCallAST() { ast_type = AST_FUNCTION_CALL; }
    Array<ExpressionAST *>args;
    TextType function_name = nullptr;
    FunctionDefinitionAST *fundef = nullptr;
};

struct DirectTypeAST : TypeAST
{
    DirectTypeAST() { ast_type = AST_DIRECT_TYPE; }

	BasicType basic_type;
    bool isSigned = false;
	TextType name = nullptr;
    TypeAST * custom_type = nullptr;
};

struct PointerTypeAST : TypeAST
{
    PointerTypeAST() { ast_type = AST_POINTER_TYPE; }
    TypeAST * points_to_type = nullptr;
};

#define ARRAY_TYPE_FLAG_C_GENERATED 0x1

struct ArrayTypeAST : TypeAST
{
    ArrayTypeAST() { ast_type = AST_ARRAY_TYPE; }
    TypeAST *array_of_type = nullptr;
    u64 num_elems = 0; // zero means static of unknown size
    ExpressionAST* num_expr = nullptr;
    bool isDynamic = false;
    u32 flags = 0;
};

struct StructTypeAST : TypeAST
{
    StructTypeAST() { ast_type = AST_STRUCT_TYPE; }
    Scope struct_scope;
    VariableDeclarationAST *decl = nullptr; // which declaration used this type, if any
};

struct StructDefinitionAST : DefinitionAST 
{
    StructDefinitionAST() { ast_type = AST_STRUCT_DEFINITION; needsSemiColon = false; }
    StructTypeAST *struct_type;
};

#define DECL_FLAG_IS_CONSTANT          0x01
#define DECL_FLAG_HAS_BEEN_INFERRED    0x02
#define DECL_FLAG_HAS_BEEN_C_GENERATED 0x04
#define DECL_FLAG_IS_FUNCTION_ARGUMENT 0x08
#define DECL_FLAG_IS_LOCAL_VARIABLE    0x10
#define DECL_FLAG_IS_GLOBAL_VARIABLE   0x20
#define DECL_FLAG_IS_TYPE              0x40
#define DECL_FLAG_IS_STRUCT_MEMBER     0x80
#define DECL_FLAG_HAS_PROTOTYPE_GEN   0x100
#define DECL_FLAG_HAS_BEEN_BT_GEN     0x200

struct VariableDeclarationAST : StatementAST
{
    VariableDeclarationAST() { ast_type = AST_VARIABLE_DECLARATION; }
    TextType varname = nullptr;
    TypeAST *specified_type = nullptr;
    DefinitionAST *definition = nullptr;
    u32 flags = 0;
    u64 bc_mem_offset = 0; 
    interp_deps *deps = nullptr; // only applicable for global variables
};

struct VarReferenceAST : ExpressionAST
{
    VarReferenceAST *next = nullptr;
    VarReferenceAST *prev = nullptr; // for errors
    u32 size_in_bytes = 0; // this is to refer to the size in bits of the whole reference 
};

struct ArrayAccessAST : VarReferenceAST
{
    ArrayAccessAST() { ast_type = AST_ARRAY_ACCESS; }
    ExpressionAST *array_exp = nullptr;
    TypeAST *access_type = nullptr;
};

struct StructAccessAST : VarReferenceAST
{
    StructAccessAST() {ast_type = AST_STRUCT_ACCESS;  }
    VariableDeclarationAST *decl = nullptr;
    TextType name = nullptr;
};

struct IdentifierAST : VarReferenceAST
{
    IdentifierAST() { ast_type = AST_IDENTIFIER; }
    VariableDeclarationAST *decl = nullptr;
    TextType name = nullptr;
};

struct LiteralAST : ExpressionAST
{
    LiteralAST() { ast_type = AST_LITERAL; }
    union {
        u64 _u64 = 0;
        s64 _s64 ;
        f64 _f64 ;
        bool _bool;
    };
    TextType str = nullptr;
    DirectTypeAST *typeAST;
};

struct BinaryOperationAST : ExpressionAST
{
    BinaryOperationAST() { ast_type = AST_BINARY_OPERATION; }
    ExpressionAST *lhs = nullptr;
    ExpressionAST *rhs = nullptr;
    TOKEN_TYPE op = TK_INVALID;
};

struct UnaryOperationAST : ExpressionAST
{
    UnaryOperationAST() { ast_type = AST_UNARY_OPERATION; }
    TOKEN_TYPE op = TK_INVALID;
    ExpressionAST *expr = nullptr;
};

struct AssignmentAST : ExpressionAST
{
    AssignmentAST() { ast_type = AST_ASSIGNMENT; }
    ExpressionAST *lhs = nullptr;
    ExpressionAST *rhs = nullptr;
    TOKEN_TYPE op = TK_INVALID;
};

void printAST(const BaseAST*ast, int ident);
const char *BasicTypeToStr(const DirectTypeAST* t);

inline bool isFunctionDeclaration(VariableDeclarationAST *decl)
{
    if (decl && (decl->specified_type) &&
        decl->specified_type->ast_type == AST_FUNCTION_TYPE) {
        return true;
    }
    return false;
}

inline bool isFunctionDefinition(VariableDeclarationAST *decl)
{
    if (decl && (decl->definition) &&
        (decl->definition->ast_type == AST_FUNCTION_DEFINITION)) {
        return true;
    }
    return false;
}

inline bool isFunctionForeign(VariableDeclarationAST *decl)
{
    if (isFunctionDeclaration(decl)) {
        auto ft = (FunctionTypeAST *)decl->specified_type;
        return ft->isForeign;
    }
    return false;
}

inline bool isStructDeclaration(VariableDeclarationAST *decl)
{
    if (decl && (decl->definition) &&
        (decl->definition->ast_type == AST_STRUCT_DEFINITION)) {
        return true;
    }
    return false;
}


inline bool isGlobalDeclaration(VariableDeclarationAST *decl)
{
    if (decl->flags & DECL_FLAG_IS_GLOBAL_VARIABLE) return true;
    return false;
}

inline bool isConstantDeclaration(VariableDeclarationAST *decl)
{
    if (decl->flags & DECL_FLAG_IS_CONSTANT) return true;
    return false;
}

inline bool isStringDeclaration(VariableDeclarationAST *decl)
{
    if (decl->specified_type->ast_type == AST_DIRECT_TYPE) {
        auto dt = (DirectTypeAST *)decl->specified_type;
        return dt->basic_type == BASIC_TYPE_STRING;
    }
    return false;
}

inline bool isStringDefinition(DefinitionAST *def)
{
    if (def->ast_type == AST_LITERAL) {
        auto lit = (LiteralAST *)def;
        return lit->typeAST->basic_type == BASIC_TYPE_STRING;
    }
    return false;
}

inline bool isStringDefinition(VariableDeclarationAST *decl)
{
    if (decl && isStringDefinition(decl->definition)) return true;
    return false;
}


inline bool isLiteral(ExpressionAST *expr) 
{
    return expr->ast_type == AST_LITERAL;
}

inline bool isVoidType(TypeAST *type)
{
    assert(type);
    if (type && type->ast_type == AST_DIRECT_TYPE) {
        auto dt = (DirectTypeAST *)type;
        return dt->basic_type == BASIC_TYPE_VOID;
    }
    return false;
}

// @TODO: These functions are very similar... maybe merge?

DirectTypeAST *findFinalDirectType(PointerTypeAST *pt);
DirectTypeAST *findFinalDirectType(ArrayTypeAST *at);
StructTypeAST *findStructType(TypeAST *type);
bool isTypeStruct(TypeAST *type);
bool isConstExpression(ExpressionAST *expr);
bool isLValue(ExpressionAST *expr, bool allowStar = true);
bool isDefinedExpression(ExpressionAST *expr);

inline bool isDirectTypeVariation(TypeAST *type) {
    return (type->ast_type == AST_DIRECT_TYPE)
        || (type->ast_type == AST_ARRAY_TYPE)
        || (type->ast_type == AST_POINTER_TYPE);
}

inline bool isTypeBoolean(TypeAST *t) 
{
    if (t->ast_type == AST_DIRECT_TYPE) {
        auto dt = (DirectTypeAST *)t;
        return dt->basic_type == BASIC_TYPE_BOOL;
    }
    return false;
}

inline bool isTypeInteger(TypeAST *t)
{
    if (t->ast_type == AST_DIRECT_TYPE) {
        auto dt = (DirectTypeAST *)t;
        return dt->basic_type == BASIC_TYPE_INTEGER;
    }
    return false;
}

inline bool isTypeFloating(TypeAST *t)
{
    if (t->ast_type == AST_DIRECT_TYPE) {
        auto dt = (DirectTypeAST *)t;
        return dt->basic_type == BASIC_TYPE_FLOATING;
    }
    return false;
}

inline bool isTypePointer(TypeAST *t)
{
    return t->ast_type == AST_POINTER_TYPE;
}

TypeAST *getDefinedType(VarReferenceAST *ast);

// Update all AST_IDENTIFIER to be possible to have a next