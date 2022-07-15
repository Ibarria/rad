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
struct LiteralAST;
struct IdentifierAST;
struct bytecode_function;
struct interp_deps;
struct BCI;

namespace llvm { class Value; class Type; class DIType; class DISubprogram; class DIScope; class BasicBlock; }

#pragma pack(push, 4)

struct Scope {
    Scope *parent = nullptr;
    Array<VariableDeclarationAST *>decls;
    Array<TypeAST *>def_types;
    FunctionDefinitionAST *current_function = nullptr;
    llvm::DIScope* debug_scope = nullptr;
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
    AST_FOR_STATEMENT,
    AST_FUNCTION_DEFINITION,
    AST_FUNCTION_CALL,
    AST_DIRECT_TYPE,
    AST_POINTER_TYPE,
    AST_ARRAY_TYPE,
    AST_NULL_TYPE,
    AST_IDENTIFIER,
    AST_LITERAL,
    AST_BINARY_OPERATION,
    AST_UNARY_OPERATION,
    AST_ASSIGNMENT,
    AST_NEW,
    AST_DELETE,
    AST_NULL_PTR,
    AST_CAST,
    AST_VARIABLE_DECLARATION,
    AST_RUN_DIRECTIVE,
    AST_STRUCT_TYPE,
    AST_STRUCT_DEFINITION,   // this is here only to satisfy class hierarchy... does not add value    
    AST_ARRAY_ACCESS,
    AST_STRUCT_ACCESS,
};

enum CastOperation {
    CASTOP_UNKNOWN = 0,
    CASTOP_NOP = 0x1000,
    CASTOP_TRUNC, 
    CASTOP_ZEXT,
    CASTOP_SEXT,
    CASTOP_FP2UI,
    CASTOP_FP2SI,
    CASTOP_SI2FP,
    CASTOP_UI2FP,
    CASTOP_FPTRUNC,
    CASTOP_FPEXT,
    CASTOP_FPBOOL,
    CASTOP_BITCAST,
    CASTOP_PTR2INT,
    CASTOP_INT2PTR
};

struct BaseAST
{
    AST_CLASS_TYPE ast_type;
    BaseAST() { ast_type = AST_UNKNOWN; }
    TextType filename = nullptr;
    Scope *scope = nullptr;
    u32 line_num = 0;  // line number of the file where this element starts
    u32 char_num = 0;  // column or character in the line where this element starts
	u64 s = 0; // This is a unique ID for this AST element
};

struct FileAST : BaseAST
{
    FileAST() { ast_type = AST_FILE; }
    Array<BaseAST *>items;
//    Array<RunDirectiveAST *>run_items;
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
    llvm::DIType *debug_type = nullptr;
    u32 type_flags = 0;
};

#define TYPE_FLAG_SIZED  0x1

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
    bool isFunctionLevel = false; // true if the block is a function body or directly. False for if, while
};

struct IfStatementAST : StatementAST
{
    IfStatementAST() { ast_type = AST_IF_STATEMENT; }
    ExpressionAST *condition = nullptr;
    StatementAST *then_branch = nullptr;
    StatementAST *else_branch = nullptr;
    llvm::Value *codegen = nullptr;
    bool then_branch_return = false;
    bool else_branch_return = false;
};

struct ForStatementAST : StatementAST
{
    ForStatementAST() { ast_type = AST_FOR_STATEMENT; }
    ExpressionAST *start = nullptr;
    ExpressionAST *end   = nullptr;
    IdentifierAST *arr   = nullptr;
    IdentifierAST *it    = nullptr;
    IdentifierAST *it_index = nullptr;
    StatementAST *loop_block = nullptr;
    Scope for_scope;
    bool is_array = false;
    bool is_it_ptr = false;
};

struct FunctionDefinitionAST : DefinitionAST
{
    FunctionDefinitionAST() { ast_type = AST_FUNCTION_DEFINITION; needsSemiColon = false; }
    FunctionTypeAST *declaration = nullptr;
    StatementBlockAST *function_body = nullptr;
    VariableDeclarationAST *var_decl = nullptr; // so that we can find the name of the function
    bytecode_function *bc_function = nullptr;
    llvm::DISubprogram *llvm_sp = nullptr;
    llvm::Value *llret_alloc = nullptr; // stores the return value mem location on llvm
    llvm::BasicBlock* llret_block = nullptr; // block where we do the return
    u32 size_in_bytes = 8;
    u32 num_return_statements = 0; // Used in llvm, number of return statements not in level
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
    bytecode_function *enclosing_func = nullptr;
    Array<RunDirectiveAST *> run_deps;
    BCI *bci = nullptr;
    bool being_generated = false;
    bool computed = false;
    bool generated = false;
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
    enum Array_Type {
        UNKNOWN_ARRAY = 0,
        STATIC_ARRAY = 1,
        SIZED_ARRAY = 2,
        DYNAMIC_ARRAY = 3
    };

    ArrayTypeAST() { ast_type = AST_ARRAY_TYPE; }
    TypeAST *array_of_type = nullptr;
    u64 num_elems = 0; // zero means static of unknown size
    ExpressionAST* num_expr = nullptr;
    Array<VariableDeclarationAST *> decls;
    Array_Type array_type = UNKNOWN_ARRAY;
    u32 flags = 0;
};

struct StructTypeAST : TypeAST
{
    StructTypeAST() { ast_type = AST_STRUCT_TYPE; }
    Scope struct_scope;
    VariableDeclarationAST *decl = nullptr; // which declaration used this type, if any
};

struct NullPtrTypeAST : TypeAST
{
    NullPtrTypeAST() { ast_type = AST_NULL_TYPE; }
};

struct StructDefinitionAST : DefinitionAST 
{
    StructDefinitionAST() { ast_type = AST_STRUCT_DEFINITION; needsSemiColon = false; }
    StructTypeAST *struct_type = nullptr;
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
    u32 llvm_index = -1; // This is only used for llvm on structs, to know the order inside a struct for GEP
                         // Also, it is used for argument counter on debug llvm
    u64 bc_offset = 0; // This can be memory offset for global variables or register offset for arguments
    interp_deps *deps = nullptr; // only applicable for global variables
};

// Which declarations are types and not actual values?
inline bool isTypeDeclaration(VariableDeclarationAST *decl)
{
    if (decl->definition) {
        return (decl->definition->ast_type == AST_FUNCTION_DEFINITION) ||
            (decl->definition->ast_type == AST_STRUCT_DEFINITION);
    }
    return false;
}

struct VarReferenceAST : ExpressionAST
{
    VarReferenceAST *next = nullptr;
    VarReferenceAST *prev = nullptr; // for errors
    LiteralAST *known_val = nullptr; // In some cases (enum, .count for array) the whole var Reference 
                                     // can be known, store such a literal and move it up
    u32 size_in_bytes = 0; // this is to refer to the size in bits of the whole reference 
};

struct ArrayAccessAST : VarReferenceAST
{
    ArrayAccessAST() { ast_type = AST_ARRAY_ACCESS; }
    ExpressionAST *array_exp = nullptr;
    TypeAST *access_type = nullptr;
} ; // __attribute__((packed));

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

struct NullPtrAST : ExpressionAST
{
    NullPtrAST() { ast_type = AST_NULL_PTR; }
    TypeAST *type_to_null = nullptr;  // LLVM demands a specific type, find it through BINOP or ASSIGN
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

struct CastAST : ExpressionAST
{
    CastAST() { ast_type = AST_CAST; }
    TypeAST *srcType = nullptr;
    TypeAST *dstType = nullptr;
    ExpressionAST *expr = nullptr;
    CastOperation castop = CASTOP_UNKNOWN;
    bool isImplicit = false;
};

struct NewAllocAST : ExpressionAST 
{
    NewAllocAST() { ast_type = AST_NEW; }
    TypeAST *type = nullptr;
};

struct DeleteAST : StatementAST
{
    DeleteAST() {ast_type = AST_DELETE; }
    ExpressionAST *expr = nullptr;
};

#pragma pack(pop)

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

inline bool isNullLiteral(ExpressionAST *expr)
{
    return expr->ast_type == AST_NULL_PTR;
}

// @TODO: These functions are very similar... maybe merge?

DirectTypeAST *findFinalDirectType(PointerTypeAST *pt);
DirectTypeAST *findFinalDirectType(ArrayTypeAST *at);
StructTypeAST *findStructType(TypeAST *type);
bool isConstExpression(ExpressionAST *expr);
bool isLValue(ExpressionAST *expr, bool allowStar = true);
bool isDefinedExpression(ExpressionAST *expr);

inline bool isTypeStruct(TypeAST *type)
{
    if (type->ast_type == AST_STRUCT_TYPE) {
        return true;
    } else if (type->ast_type == AST_DIRECT_TYPE) {
        auto dt = (DirectTypeAST *)type;
        if (dt->basic_type != BASIC_TYPE_CUSTOM) {
            return false; // normal direct types are not structs
        }

        // if we are here, we should have computed the custom type
        assert(dt->custom_type);
        // This allows for more than 1 level of indirection
        return isTypeStruct(dt->custom_type);
    } 
    return false;
}


inline bool isDirectTypeVariation(TypeAST *type) {
    return (type->ast_type == AST_DIRECT_TYPE)
        || (type->ast_type == AST_ARRAY_TYPE)
        || (type->ast_type == AST_POINTER_TYPE);
}

inline bool isDirectType(TypeAST *type) {
    return (type->ast_type == AST_DIRECT_TYPE);
}

inline bool isTypeArray(TypeAST *type) {
    return type->ast_type == AST_ARRAY_TYPE;
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

inline bool isTypeNullPtr(TypeAST *t)
{
    return t->ast_type == AST_NULL_TYPE;
}

// Should this include the VOID type?
inline bool isTypeRunSupported(TypeAST *t) 
{
    if (t->ast_type == AST_DIRECT_TYPE) {
        auto dt = (DirectTypeAST *)t;
        return (dt->basic_type == BASIC_TYPE_BOOL) ||
            (dt->basic_type == BASIC_TYPE_INTEGER) ||
            (dt->basic_type == BASIC_TYPE_FLOATING);
    }
    return false;
}

inline bool isStaticArray(ArrayTypeAST *at)
{
    return at->array_type == ArrayTypeAST::STATIC_ARRAY;
}

inline bool isSizedArray(ArrayTypeAST *at)
{
    return at->array_type == ArrayTypeAST::SIZED_ARRAY;
}

inline bool isDynamicArray(ArrayTypeAST *at)
{
    return at->array_type == ArrayTypeAST::DYNAMIC_ARRAY;
}

inline bool isStaticArray(TypeAST *t)
{
    if (isTypeArray(t)) {
        return isStaticArray((ArrayTypeAST *)t);
    }
    return false;
}

inline bool isSizedArray(TypeAST *t)
{
    if (isTypeArray(t)) {
        return isSizedArray((ArrayTypeAST *)t);
    }
    return false;
}

inline TypeAST *getDefinedType(VarReferenceAST *ast)
{
    switch (ast->ast_type) {
    case AST_ARRAY_ACCESS: {
        auto ac = (ArrayAccessAST *)ast;
        return ac->access_type;
        break;
    }
    case AST_STRUCT_ACCESS: {
        auto sac = (StructAccessAST *)ast;
        if (!sac->decl) return nullptr;
        assert(sac->decl);
        return sac->decl->specified_type;
        break;
    }
    case AST_IDENTIFIER: {
        auto id = (IdentifierAST *)ast;
        if (!id->decl) return nullptr;
        assert(id->decl);
        return id->decl->specified_type;
        break;
    }
    default: assert(!"Invalid AST type for a reference");
        return nullptr;
    }
}

const char *AstClassTypeToStr(AST_CLASS_TYPE atype);
FunctionDefinitionAST* findEnclosingFunction(BaseAST* ast);

// Update all AST_IDENTIFIER to be possible to have a next