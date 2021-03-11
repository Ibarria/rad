#include "Parser.h"
#include "Lexer.h"
#include "Interpreter.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#ifndef WIN32
# define sprintf_s  sprintf
# define vsprintf_s vsnprintf
# define strncpy_s  strncpy
#endif

extern bool option_printTokens;

#define NEW_AST(ast_type) (ast_type *) setASTinfo(this, (BaseAST *) new(this->pool) ast_type )

u64 sequence_id = 100;

void copyASTinfo(BaseAST *src, BaseAST *dst) 
{
    dst->filename = src->filename;
    dst->line_num = src->line_num;
    dst->char_num = src->char_num;
    dst->s = sequence_id++;
    dst->scope = src->scope;
}

static BaseAST * setASTloc(Parser *p, BaseAST *ast)
{
    SrcLocation loc;
    p->lex->getLocation(loc);
    ast->line_num = loc.line;
    ast->char_num = loc.col;
    ast->filename = p->lex->getFilename();
	// @TODO: make this an atomic operation
	ast->s = sequence_id++;
    return ast;
}

static BaseAST * setASTinfo(Parser *p, BaseAST *ast)
{
    setASTloc(p, ast);
    ast->scope = p->current_scope;
    return ast;
}

void Parser::ErrorWithLoc(SrcLocation &loc, const char *msg, ...)
{
    va_list args;
    s32 off = sprintf(errorString, "%s:%d:%d: error : ", lex->getFilename(),
        loc.line, loc.col);

    va_start(args, msg);
    off += vsprintf(errorString + off, msg, args);
    va_end(args);
    success = false;
    errorString += off;
    errorString = lex->getFileData()->printLocation(loc, errorString);
}

void Parser::Error(const char *msg, ...)
{
    va_list args;
    SrcLocation loc;
    lex->getLocation(loc);
    s32 off = sprintf(errorString, "%s:%d:%d: error : ", lex->getFilename(), 
        loc.line, loc.col);

    va_start(args, msg);
    off += vsprintf(errorString + off, msg, args);
    va_end(args);
    success = false;
    errorString += off;
    errorString = lex->getFileData()->printLocation(loc, errorString);
}

bool Parser::MustMatchToken(TOKEN_TYPE type, const char *msg)
{
    if (!lex->checkToken(type)) {
        if (type == TK_SEMICOLON) {
            Token tok;
            lex->lookbehindToken(tok);
            ErrorWithLoc(tok.loc, "%s - Expected a semicolon after this token\n", msg);
            return false;
        }
        Error("%s - Token %s was expected, but we found: %s\n", msg,
            TokenTypeToStr(type), TokenTypeToStr(lex->getTokenType()) );
        return false;
    }
    lex->consumeToken();
    return true;
}

bool Parser::AddDeclarationToScope(VariableDeclarationAST * decl)
{
    for (auto d : current_scope->decls) {
        if (d->varname == decl->varname) {
            Error("Variable [%s] is already defined in the scope", decl->varname);
            return false;
        }
    }
    current_scope->decls.push_back(decl);
    if (!(decl->flags & DECL_FLAG_IS_FUNCTION_ARGUMENT)) {
        // argument declarations have their flag set already
        if (current_scope->parent == nullptr) {
            decl->flags |= DECL_FLAG_IS_GLOBAL_VARIABLE;
        } else {
            decl->flags |= DECL_FLAG_IS_LOCAL_VARIABLE;
        }
    }
    return true;
}

bool Parser::AddDeclarationToStruct(StructDefinitionAST * struct_def, VariableDeclarationAST * decl)
{
    for (auto d : struct_def->struct_type->struct_scope.decls) {
        if (d->varname == decl->varname) {
            Error("Variable %s is already defined within this struct", decl->varname);
            return false;
        }
    }
    struct_def->struct_type->struct_scope.decls.push_back(decl);
    decl->scope = &struct_def->struct_type->struct_scope;
    decl->flags |= DECL_FLAG_IS_STRUCT_MEMBER;
    return true;
}

static bool isIntegerType(BasicType t)
{
    return (t == BASIC_TYPE_INTEGER);
}

static bool isFloatType(BasicType t)
{
    return (t == BASIC_TYPE_FLOATING);
}

static bool isUnaryPrefixOperator(TOKEN_TYPE type) 
{
    return (type == TK_PLUS)
        || (type == TK_MINUS)
        || (type == TK_BANG)
        || (type == TK_STAR)
        || (type == TK_LSHIFT);
}

static bool isAssignmentOperator(TOKEN_TYPE type)
{
    return (type == TK_ASSIGN)
        || (type == TK_MUL_ASSIGN)
        || (type == TK_DIV_ASSIGN)
        || (type == TK_MOD_ASSIGN)
        || (type == TK_ADD_ASSIGN)
        || (type == TK_SUB_ASSIGN)
        || (type == TK_AND_ASSIGN)
        || (type == TK_XOR_ASSIGN)
        || (type == TK_OR_ASSIGN);
}

static bool isBinOperator(TOKEN_TYPE type)
{
    return (type == TK_EQ)
        || (type == TK_LEQ)
        || (type == TK_GEQ)
        || (type == TK_NEQ)
        || (type == TK_LT)
        || (type == TK_GT)
        || (type == TK_STAR)
        || (type == TK_DIV)
        || (type == TK_MOD)
        || (type == TK_PIPE)
        || (type == TK_DOUBLE_PIPE)
        || (type == TK_HAT)
        || (type == TK_AMP)
        || (type == TK_DOUBLE_AMP)
        || (type == TK_PLUS)
        || (type == TK_MINUS);
}

bool isBoolOperator(TOKEN_TYPE type)
{
    return (type == TK_EQ)
        || (type == TK_LEQ)
        || (type == TK_GEQ)
        || (type == TK_NEQ)
        || (type == TK_LT)
        || (type == TK_GT);
}


static u32 getPrecedence(TOKEN_TYPE t)
{
    switch (t) {
    case TK_STAR:
    case TK_DIV:
    case TK_MOD:
        return 1;
    case TK_MINUS:
    case TK_PLUS:
        return 2;
    case TK_LT:
    case TK_GT:
    case TK_LEQ:
    case TK_GEQ:
        return 4;
    case TK_EQ:
    case TK_NEQ:
        return 5;
    case TK_AMP:
        return 6;
    case TK_HAT:
        return 7;
    case TK_PIPE:
        return 8;
    case TK_DOUBLE_AMP:
        return 9;
    case TK_DOUBLE_PIPE:
        return 10;
    }
    return 0;
}

static bool isBuiltInType(TOKEN_TYPE t)
{
    return (t == TK_BOOL)
        || (t == TK_INT)
        || (t == TK_U8)
        || (t == TK_U16)
        || (t == TK_U32)
        || (t == TK_U64)
        || (t == TK_S8)
        || (t == TK_S16)
        || (t == TK_S32)
        || (t == TK_S64)
        || (t == TK_FLOAT)
        || (t == TK_F32)
        || (t == TK_F64)
        || (t == TK_STRING_KEYWORD)
        || (t == TK_VOID);
}

static bool isHexNumber(Token &t)
{
    if (t.type == TK_NUMBER) {
        return strcmp("0x", t.string) == 0;
    }
    return false;
}

static f64 computeDecimal(Token &t)
{
    assert(t.type == TK_NUMBER);
    assert(!isHexNumber(t));

    double total = 0;
    double divisor = 10;
    char *s = t.string;
    while (*s != 0) {
        float num = (float)(*s - '0');        
        total += num / divisor;
        divisor *= 10;
        s++;
    }

    return total;
}

static struct TypeHelperRec {
    TOKEN_TYPE tk;
    const char *name;
    u64 bytes;
    BasicType bt;
    bool sign;
    DirectTypeAST *ast;
} TypeHelper[] = {
    { TK_VOID, "void", 0, BASIC_TYPE_VOID, false, nullptr },
    { TK_BOOL, "bool", 1, BASIC_TYPE_BOOL, false, nullptr },
    { TK_STRING_KEYWORD, "string", 16, BASIC_TYPE_STRING, false, nullptr },
    { TK_U8,  "u8",  1, BASIC_TYPE_INTEGER, false, nullptr },
    { TK_U16, "u16", 2, BASIC_TYPE_INTEGER, false, nullptr },
    { TK_U32, "u32", 4, BASIC_TYPE_INTEGER, false, nullptr },
    { TK_U64, "u64", 8, BASIC_TYPE_INTEGER, false, nullptr },
    { TK_S8,  "s8",  1, BASIC_TYPE_INTEGER, true, nullptr },
    { TK_S16, "s16", 2, BASIC_TYPE_INTEGER, true, nullptr },
    { TK_S32, "s32", 4, BASIC_TYPE_INTEGER, true, nullptr },
    { TK_S64, "s64", 8, BASIC_TYPE_INTEGER, true, nullptr },
    { TK_INT, "int", 8, BASIC_TYPE_INTEGER, true, nullptr },
    { TK_F32, "f32", 4, BASIC_TYPE_FLOATING, true, nullptr },
    { TK_F64, "f64", 8, BASIC_TYPE_FLOATING, true, nullptr },
    { TK_FLOAT, "float", 8, BASIC_TYPE_FLOATING, true, nullptr },
    { TK_INVALID, nullptr, 0, BASIC_TYPE_VOID, false, nullptr }
};

DirectTypeAST *getTypeEx(DirectTypeAST *oldtype, u32 newbytes)
{
    TypeHelperRec *th;
    for (th = TypeHelper; th->name != nullptr; th++) {
        if ((th->bt == oldtype->basic_type) && (th->sign == oldtype->isSigned)
            && (th->bytes == newbytes)) {
            return th->ast;
        }
    }
    assert(false);
    return nullptr;
}

DirectTypeAST *getBuiltInType(TOKEN_TYPE tktype)
{
    if ((tktype >= TK_VOID) && (tktype <= TK_FLOAT)) {
        return TypeHelper[tktype - TK_VOID].ast;
    }

    assert(false);
    return nullptr;
}

void Parser::defineBuiltInTypes()
{
    if (TypeHelper[0].ast != nullptr) return;

    TypeHelperRec *th;
    for (th = TypeHelper; th->name != nullptr; th++) {
        DirectTypeAST *tp;

        tp = createType(th->tk, CreateTextType(pool, th->name));
        th->ast = tp;
    }

    assert(TypeHelper[TK_FLOAT - TK_VOID].tk == TK_FLOAT);
}

DirectTypeAST *Parser::createType(TOKEN_TYPE tktype, TextType name)
{
    DirectTypeAST *type = (DirectTypeAST *) new(this->pool) DirectTypeAST;
    type->s = sequence_id++;

    type->name = name;
    switch (tktype) {
    case TK_VOID:
        type->basic_type = BASIC_TYPE_VOID;
        type->size_in_bytes = 0; // Basically this can never be a direct type
        break;
    case TK_BOOL:
        type->basic_type = BASIC_TYPE_BOOL;
        type->size_in_bytes = 1; // This could change, but enough for now
        break;
    case TK_STRING_KEYWORD:
        type->basic_type = BASIC_TYPE_STRING;
        type->size_in_bytes = 8 + 8; // for the pointer to data and the size of the string
        break;
    case TK_U8:
        type->basic_type = BASIC_TYPE_INTEGER;
        type->size_in_bytes = 1;
        break;
    case TK_U16:
        type->basic_type = BASIC_TYPE_INTEGER;
        type->size_in_bytes = 2;
        break;
    case TK_U32:
        type->basic_type = BASIC_TYPE_INTEGER;
        type->size_in_bytes = 4;
        break;
    case TK_U64:
        type->basic_type = BASIC_TYPE_INTEGER;
        type->size_in_bytes = 8;
        break;
    case TK_S8:
        type->basic_type = BASIC_TYPE_INTEGER;
        type->isSigned = true;
        type->size_in_bytes = 1;
        break;
    case TK_S16:
        type->basic_type = BASIC_TYPE_INTEGER;
        type->isSigned = true;
        type->size_in_bytes = 2;
        break;
    case TK_S32:
        type->basic_type = BASIC_TYPE_INTEGER;
        type->isSigned = true;
        type->size_in_bytes = 4;
        break;
    case TK_INT:
    case TK_S64:
        type->basic_type = BASIC_TYPE_INTEGER;
        type->isSigned = true;
        type->size_in_bytes = 8;
        break;
    case TK_F32:
        type->basic_type = BASIC_TYPE_FLOATING;
        type->size_in_bytes = 4;
        type->isSigned = true;
        break;
    case TK_FLOAT:
    case TK_F64:
        type->basic_type = BASIC_TYPE_FLOATING;
        type->size_in_bytes = 8;
        type->isSigned = true;
        break;
    case TK_IDENTIFIER:
        type->basic_type = BASIC_TYPE_CUSTOM;
        break;
    default:
        assert(!"Identifier types, custom types are not implemented");
    }

    return type;
}

DirectTypeAST *Parser::getType(TOKEN_TYPE tktype, TextType name)
{
    if ((tktype >= TK_VOID) && (tktype <= TK_FLOAT)) {
        return TypeHelper[tktype - TK_VOID].ast;
    }

    DirectTypeAST *type = NEW_AST(DirectTypeAST);
    type->name = name;
    switch (tktype) {
    case TK_IDENTIFIER:
        type->basic_type = BASIC_TYPE_CUSTOM;
        break;
    default:
        assert(!"Identifier types, custom types are not implemented");
    }

    return type;
}

TypeAST *Parser::parseDirectType()
{
    // @TODO: support pointer, arrays, etc
    Token t;
    lex->getNextToken(t);
    if ((t.type == TK_IDENTIFIER) || isBuiltInType(t.type)) {
        return getType(t.type, t.string);
    } else if (t.type == TK_STAR) {
        // This is a pointer to something
        PointerTypeAST *pt = NEW_AST(PointerTypeAST);
        pt->points_to_type = parseDirectType();
        pt->size_in_bytes = 8;
        return pt;
    } else if (t.type == TK_OPEN_SQBRACKET) {
        // this is an array declaration
        // The only supported options are: [] , [..] , [constant number expression]
        // option 3 is evaluated at Interpreter time
        ArrayTypeAST *at = NEW_AST(ArrayTypeAST);
        
        VariableDeclarationAST *data_decl = NEW_AST(VariableDeclarationAST);
        PointerTypeAST *pt = NEW_AST(PointerTypeAST);
        pt->points_to_type = nullptr; // will be filled later
        pt->size_in_bytes = 8;
        data_decl->specified_type = pt;
        data_decl->varname = CreateTextType(pool, "data");
        at->decls.push_back(data_decl);

        VariableDeclarationAST *count_decl = NEW_AST(VariableDeclarationAST);
        count_decl->specified_type = getType(TK_U64, nullptr);
        count_decl->varname = CreateTextType(pool, "count");
        at->decls.push_back(count_decl);

        if (lex->checkToken(TK_CLOSE_SQBRACKET)) {
            lex->consumeToken();
            at->array_type = ArrayTypeAST::SIZED_ARRAY;
        } else if (lex->checkToken(TK_DOUBLE_PERIOD)) {
            lex->consumeToken();
            MustMatchToken(TK_CLOSE_SQBRACKET, "Declaration of array type needs a closed square bracket");
            if (!success) {
                return nullptr;
            }
            at->array_type = ArrayTypeAST::DYNAMIC_ARRAY;
            VariableDeclarationAST *rsize_decl = NEW_AST(VariableDeclarationAST);
            rsize_decl->specified_type = getType(TK_U64, nullptr);
            rsize_decl->varname = CreateTextType(pool, "reserved_size");
            at->decls.push_back(rsize_decl);

        } else {
            at->num_expr = parseExpression();
            if (!success) {
                return nullptr;
            }
            MustMatchToken(TK_CLOSE_SQBRACKET, "Declaration of array type needs a closed square bracket");
            if (!success) {
                return nullptr;
            }
            at->array_type = ArrayTypeAST::STATIC_ARRAY;
        }
        at->array_of_type = parseType();
        if (!success) {
            return nullptr;
        }
        pt->points_to_type = at->array_of_type;
        return at;
    } else {
        if (t.type == TK_STRUCT) {
            Error("To declare a struct you need to use the form of <var> := struct { ... }");
        } else {
            Error("Variable type token could not be found, but we found: %s\n",
                TokenTypeToStr(t.type));
        }
        return nullptr;
    }
}

TypeAST * Parser::parseType()
{
    if (lex->checkToken(TK_OPEN_PAREN)) {
        return parseFunctionDeclaration();
    }

    return parseDirectType();
}

VariableDeclarationAST *Parser::parseArgumentDeclaration()
{
    Token t;
    lex->lookaheadToken(t);
    VariableDeclarationAST *arg = NEW_AST(VariableDeclarationAST);
    MustMatchToken(TK_IDENTIFIER, "Argument declaration needs to start with an identifier");
    if (!success) {
        return nullptr;
    }
    arg->varname = t.string;

    MustMatchToken(TK_COLON, "Argument declaration needs a colon between identifier and type");

    if (!success) {
        return nullptr;
    }

    arg->specified_type = parseType();

    if (!success) {
        return nullptr;
    }
    arg->flags |= DECL_FLAG_IS_FUNCTION_ARGUMENT;
    return arg;
}

FunctionTypeAST *Parser::parseFunctionDeclaration()
{
    MustMatchToken(TK_OPEN_PAREN, "Function declarations need to start with an open parenthesis");
    if (!success) {
        return nullptr;
    }

    FunctionTypeAST *fundec = NEW_AST(FunctionTypeAST);

    while (!lex->checkToken(TK_CLOSE_PAREN)) {
        if (lex->checkToken(TK_DOUBLE_PERIOD)) {
            // this is a special argument case, the variable lenght argument
            lex->consumeToken();
            fundec->hasVariableArguments = true;

            if (!lex->checkToken(TK_CLOSE_PAREN)) {
                Error("Variable lenght arguments must be the last parameter in a function\n");
                return nullptr;
            }
            continue;
        }
        VariableDeclarationAST *arg = Parser::parseArgumentDeclaration();
        if (!success) {
            return nullptr;
        }

        fundec->arguments.push_back(arg);
        if (lex->checkToken(TK_COMMA)) {
            lex->consumeToken();
        } else if (!lex->checkToken(TK_CLOSE_PAREN)) {
            Error("Comma must be used in between parameters in a function\n");
            return nullptr;
        }
    }
    lex->consumeToken();
    // Now do the return value, which can be empty
    if (lex->checkToken(TK_RETURN_ARROW)) {
        lex->consumeToken();
        fundec->return_type = parseType();
    } else {
        fundec->return_type = getType(TK_VOID, (char *)"void");
    }
    if (!success) {
        return nullptr;
    }
    return fundec;
}

ReturnStatementAST *Parser::parseReturnStatement()
{
    ReturnStatementAST *ret = NEW_AST(ReturnStatementAST);
    MustMatchToken(TK_RETURN);
    if (!success) {
        return nullptr;
    }

    ret->ret = parseExpression();
    if (!success) {
        return nullptr;
    }
    MustMatchToken(TK_SEMICOLON, "Statement needs to end in semicolon");
    if (!success) {
        return nullptr;
    }
    return ret;
}

IfStatementAST * Parser::parseIfStatement()
{
    IfStatementAST *ifst = NEW_AST(IfStatementAST);
    MustMatchToken(TK_IF);
    if (!success) {
        return nullptr;
    }

    ifst->condition = parseExpression();
    if (!success) {
        return nullptr;
    }

    ifst->then_branch = parseStatement();
    if (!success) {
        return nullptr;
    }

    TOKEN_TYPE cur_type;
    cur_type = lex->getTokenType();
    if (cur_type == TK_ELSE) {
        lex->consumeToken();
        ifst->else_branch = parseStatement();
        if (!success) {
            return nullptr;
        }
    }

    return ifst;
}

static IdentifierAST *checkArrayIterator(ExpressionAST *expr, bool &isPtr)
{
    isPtr = false;
    if (expr->ast_type == AST_IDENTIFIER) return (IdentifierAST *)expr;
    if (expr->ast_type == AST_UNARY_OPERATION) {
        auto unop = (UnaryOperationAST *)expr;
        if (unop->op != TK_STAR) return nullptr;
        if (unop->expr->ast_type == AST_IDENTIFIER) {
            isPtr = true;
            return (IdentifierAST *)unop->expr;
        }
    }
    return nullptr;
}

ForStatementAST * Parser::parseForStatement()
{
    ForStatementAST *forst = NEW_AST(ForStatementAST);
    MustMatchToken(TK_FOR);
    if (!success) {
        return nullptr;
    }
    forst->is_array = true;
    ExpressionAST *expr = parseExpression();
    if (!success) {
        return nullptr;
    }

    TOKEN_TYPE cur_type;
    cur_type = lex->getTokenType();
    switch (cur_type) {
    case TK_DOUBLE_PERIOD: {
        lex->consumeToken();
        // this is the case where we iterate in a range
        forst->is_array = false;
        forst->start = expr;
        forst->end = parseExpression();
        if (!success) {
            return nullptr;
        }
        break;
    } 
    case TK_COLON:
    case TK_COMMA: {
        lex->consumeToken();
        // When we see a comma, means we are going to have named iterator and index
        forst->it = checkArrayIterator(expr, forst->is_it_ptr);
        if (forst->it == nullptr) {
            Error("Iterator on the for loop has to be an identifier");
            return nullptr;
        }
        if (forst->it->next) {
            Error("Iterator on the for loop has to be a simple identifier");
            return nullptr;
        }

        if (cur_type == TK_COMMA) {
            expr = parseExpression();
            if (!success) {
                return nullptr;
            }
            if (expr->ast_type != AST_IDENTIFIER) {
                Error("Iterator index on the for loop has to be an identifier");
                return nullptr;
            }
            forst->it_index = (IdentifierAST *)expr;
            if (forst->it_index->next) {
                Error("Iterator index on the for loop has to be a simple identifier");
                return nullptr;
            }
            if (!lex->checkToken(TK_COLON)) {
                Error("For loop with iterator and index needs to be followed by \": <array> ");
                return nullptr;
            }
            lex->consumeToken();
        }

        // Here we parse the range, which can be an array or start .. end
        expr = parseExpression();
        if (!success) {
            return nullptr;
        }

        if (lex->checkToken(TK_DOUBLE_PERIOD)) {
            // we are in a range case
            forst->is_array = false;
            // first, store the previous expression in start
            forst->start = expr;
            lex->consumeToken();
            forst->end = parseExpression();
            if (!success) {
                return nullptr;
            }
        } else {
            // the current expression has to refer to an array
            if (expr->ast_type != AST_IDENTIFIER) {
                Error("For array variable has to be an identifier");
                return nullptr;
            }
            forst->arr = (IdentifierAST *)expr;
        }
        break;
    } 
    default: {
        // We must have seen something that is the for block, so assume expr is the array
        if (expr->ast_type != AST_IDENTIFIER) {
            Error("For array variable has to be an identifier");
            return nullptr;
        }
        forst->arr = (IdentifierAST *)expr;
        if (!success) {
            return nullptr;
        }
    }
    }
    
    forst->for_scope.parent = current_scope;
    current_scope = &forst->for_scope;

    // Assume single array, time to parse a statement
    forst->loop_block = parseStatement();

    current_scope = current_scope->parent;

    if (!success) {
        return nullptr;
    }

    return forst;
}

StatementAST *Parser::parseStatement()
{
    TOKEN_TYPE cur_type;
    StatementAST *statement = nullptr;

    cur_type = lex->getTokenType();
    if (cur_type == TK_IDENTIFIER) {
        // could be a variable definition or a statement
        if (lex->checkAheadToken(TK_COLON,1)
            || lex->checkAheadToken(TK_DOUBLE_COLON, 1)
            || lex->checkAheadToken(TK_IMPLICIT_ASSIGN, 1)) {
            statement = parseDeclaration();
        } else if (lex->checkAheadToken(TK_OPEN_PAREN, 1)) {
            // this is a function call, parse it as such
            statement = parseFunctionCall();
            if (!success) {
                return nullptr;
            }
            MustMatchToken(TK_SEMICOLON, "Statement needs to end in semicolon");
            if (!success) {
                return nullptr;
            }
        } else {
            statement = parseAssignmentOrExpression();
            if (!success) {
                return nullptr;
            }
            MustMatchToken(TK_SEMICOLON, "Statement needs to end in semicolon");
            if (!success) {
                return nullptr;
            }
        }
        return statement;
    } else if (cur_type == TK_OPEN_BRACKET) {
        return parseStatementBlock();
    } else if (cur_type == TK_RETURN) {
        return parseReturnStatement();
    } else if (cur_type == TK_IF) {
        return parseIfStatement();
    } else if (cur_type == TK_FOR) {
        return parseForStatement();
    } else if (cur_type == TK_WHILE) {
        Error("The `while` statement is not yet supported.\n");
        return nullptr;
    } else if (cur_type == TK_ELSE) {
        Error("Found a mismatched `else`.\n");
        return nullptr;
    } else {
        statement = parseAssignmentOrExpression();
        MustMatchToken(TK_SEMICOLON, "Statement needs to end in semicolon");
        if (!success) {
            return nullptr;
        }
        return statement;
    }
}

StatementBlockAST *Parser::parseStatementBlock(FunctionDefinitionAST *fundef)
{
    if (!lex->checkToken(TK_OPEN_BRACKET)) {
        Error("We are trying to parse a statement block and it needs to start with an open bracket\n");
        return nullptr;
    }
    lex->consumeToken(); // consume the {
    StatementBlockAST *block = NEW_AST(StatementBlockAST);

    // push scope
    block->block_scope.parent = current_scope;
    current_scope = &block->block_scope;

    if (fundef) {
        current_scope->current_function = fundef;
        // if this is the body of a function, register the arguments as variables
        for(auto arg:fundef->declaration->arguments) {
            AddDeclarationToScope(arg);
            arg->scope = current_scope;
        }
    }
    
    while (!lex->checkToken(TK_CLOSE_BRACKET)) {
        StatementAST *statement = nullptr;
        statement = parseStatement();
        if (!success) {
            return nullptr;
        }
        block->statements.push_back(statement);
        if (lex->checkToken(TK_LAST_TOKEN)) {
            Error("Failed to find a matching close bracket, open bracket at line %d\n", block->line_num);
            if (!success) {
                return nullptr;
            }
        }
    }
    lex->consumeToken(); // match }
    // pop scope
    current_scope = current_scope->parent;
    return block;
}


FunctionDefinitionAST *Parser::parseFunctionDefinition()
{
    FunctionDefinitionAST *fundef = NEW_AST(FunctionDefinitionAST);

    fundef->declaration = parseFunctionDeclaration();
    if (!success) {
        return nullptr;
    }
    if (isImport && lex->checkToken(TK_FOREIGN)) {
        // foreign functions are special, there is no body for them
        fundef->declaration->isForeign = true;
        lex->consumeToken();
        MustMatchToken(TK_SEMICOLON, "Function definitions need to end in semicolon\n");
        return fundef;
    }
    if (!lex->checkToken(TK_OPEN_BRACKET)) {        
        Error("Function declaration needs to be followed by an implementation {}, found token: %s \n",
            TokenTypeToStr(lex->getTokenType()));
        return nullptr;
    }
    // We need to add the declarated variables into the statementBlock
    // for the function
    fundef->function_body = parseStatementBlock(fundef);
    if (!success) {
        return nullptr;
    }
    return fundef;
}

// This is really the declaration... 
StructDefinitionAST * Parser::parseStructDefinition()
{
    StructDefinitionAST *struct_def = NEW_AST(StructDefinitionAST);
    struct_def->struct_type = NEW_AST(StructTypeAST);
    Token t;
    lex->getNextToken(t);

    assert(t.type == TK_STRUCT);

    // Add here possible support for SOA or other qualifiers
    lex->getNextToken(t);

    if (t.type == TK_OPEN_BRACKET) {
        while (t.type != TK_CLOSE_BRACKET) {
            // now we process the different elements inside the struct, recursively
            VariableDeclarationAST *decl = parseDeclaration(true);
            if (!success) {
                return nullptr;
            }
            AddDeclarationToStruct(struct_def, decl);
            if (!success) {
                return nullptr;
            }
            
            lex->lookaheadToken(t);
        }
        // consume the close bracket
        lex->consumeToken();
    }

    return struct_def;
}

FunctionCallAST * Parser::parseFunctionCall()
{
    FunctionCallAST *funcall = NEW_AST(FunctionCallAST);
    Token t;
    lex->getNextToken(t);

    assert(t.type == TK_IDENTIFIER);
    funcall->function_name = t.string;

    MustMatchToken(TK_OPEN_PAREN);
    if (!success) {
        return nullptr;
    }
    while (!lex->checkToken(TK_CLOSE_PAREN)) {
        ExpressionAST * expr = parseExpression();
        if (!success) {
            return nullptr;
        }
        funcall->args.push_back(expr);
        if (!lex->checkToken(TK_CLOSE_PAREN)) {
            if (lex->checkToken(TK_COMMA)) {
                lex->consumeToken();
            } else {
                Error("Comma must be used to separate function arguments\n");
                return nullptr;
            }
        }
    }

    MustMatchToken(TK_CLOSE_PAREN);
    if (!success) {
        return nullptr;
    }

    return funcall;
}

VarReferenceAST * Parser::parseVarReference()
{
    Token t;
    lex->getCurrentToken(t);

    if (t.type == TK_OPEN_SQBRACKET) {
        lex->consumeToken();
        // This is an array access
        ArrayAccessAST *acc = NEW_AST(ArrayAccessAST);

        acc->array_exp = parseExpression();
        if (!success) {
            return nullptr;
        }
        MustMatchToken(TK_CLOSE_SQBRACKET, "Cound not find matching close square bracket");
        if (!success) {
            return nullptr;
        }
        acc->next = parseVarReference();
        if (acc->next) {
            acc->next->prev = acc;
        }
        return acc;
    }

    if (t.type == TK_PERIOD) {
        lex->consumeToken();
        // compound statement
        StructAccessAST *sac = NEW_AST(StructAccessAST);

        lex->getNextToken(t);
        if (t.type != TK_IDENTIFIER) {
            Error("An identifier must follow after a period access expression");
            return nullptr;
        }

        sac->name = t.string;
        sac->next = parseVarReference();
        if (sac->next) {
            sac->next->prev = sac;
            // the scope for a variable reference is that of the enclosing struct
            // since we do not yet know it, just null it to be safe
            sac->next->scope = nullptr;
        }
        return sac;
    } 
    // This is not an error, just that the VarReference has ended
    return nullptr;
}

ExpressionAST * Parser::parseLiteral()
{
    Token t;
    lex->getNextToken(t);

    if (t.type == TK_IDENTIFIER) {
        IdentifierAST *ex = NEW_AST(IdentifierAST);
        ex->name = t.string;
        ex->next = parseVarReference();
        if (ex->next) {
            ex->next->prev = ex;
        }
        return ex;
    } else if (t.type == TK_NULL) {
        auto nptr = NEW_AST(NullPtrAST);
        nptr->expr_type = NEW_AST(NullPtrTypeAST);
        nptr->expr_type->size_in_bytes = 8;
        return nptr;
    } else if ((t.type == TK_NUMBER) || (t.type == TK_TRUE) ||
        (t.type == TK_FNUMBER) || (t.type == TK_STRING) || (t.type == TK_FALSE)) {
        auto ex = NEW_AST(LiteralAST);
        // setASTinfo(this, ex->typeAST);

        if (t.type == TK_NUMBER) {
            ex->typeAST = getType(TK_U64, nullptr);
            //ex->typeAST.basic_type = BASIC_TYPE_INTEGER;
            //ex->typeAST.size_in_bytes = 8;
            ex->_u64 = t._u64;
        } else if (t.type == TK_FNUMBER) {
            ex->typeAST = getType(TK_FLOAT, nullptr);
            //ex->typeAST.basic_type = BASIC_TYPE_FLOATING;
            //ex->typeAST.size_in_bytes = 8;
            ex->_f64 = t._f64;
        } else if (t.type == TK_STRING) {
            ex->typeAST = getType(TK_STRING_KEYWORD, nullptr);
            //ex->typeAST.basic_type = BASIC_TYPE_STRING;
            //ex->typeAST.size_in_bytes = 8+8;
            ex->str = t.string;
        } else if (t.type == TK_TRUE) {
            ex->typeAST = getType(TK_BOOL, nullptr);
            //ex->typeAST.basic_type = BASIC_TYPE_BOOL;
            //ex->typeAST.size_in_bytes = 1;
            ex->_bool = true;
        } else if (t.type == TK_FALSE) {
            ex->typeAST = getType(TK_BOOL, nullptr);
            //ex->typeAST.basic_type = BASIC_TYPE_BOOL;
            //ex->typeAST.size_in_bytes = 1;
            ex->_bool = false;
        }
        return ex;
    } else if (t.type == TK_OPEN_PAREN) {
        SrcLocation loc;
        loc = t.loc;

        ExpressionAST *expr = parseExpression();
        if (!success) return nullptr;

        lex->getNextToken(t);
        if (t.type != TK_CLOSE_PAREN) {
            Error("Cound not find a matching close parentesis, open parenthesis was at %d:%d\n",
                loc.line, loc.col);
            if (!success) {
                return nullptr;
            }
        }

        return expr;
    } else if (t.type == TK_PERIOD) {
        // We support period for expressions like `.5`
        if (lex->checkAheadToken(TK_NUMBER, 1)) {
            lex->getNextToken(t);

            if (isHexNumber(t)) {
                Error("After a period we need to see normal numbers, not hex numbers");
                return nullptr;
            }

            auto ex = NEW_AST(LiteralAST);

            ex->typeAST = getType(TK_FLOAT, nullptr);
            ex->_f64 = computeDecimal(t);
            return ex;
        } else {
            Error("Could not parse a literal expression! Unknown token type: %s", TokenTypeToStr(t.type));
            return nullptr;
        }
    }
    Error("Could not parse a literal expression! Unknown token type: %s", TokenTypeToStr(t.type));
    return nullptr;
}

ExpressionAST * Parser::parseUnaryExpression()
{
    Token t;
    lex->lookaheadToken(t);

    if (t.type == TK_IDENTIFIER) {
        if (lex->checkAheadToken(TK_OPEN_PAREN, 1)) {
            return parseFunctionCall();
        }
    } else if (t.type == TK_NEW) {
        lex->consumeToken();
        NewAllocAST *nast = NEW_AST(NewAllocAST);
        nast->type = parseType();
        if (!success) return nullptr;

        return nast;    
    } else if (isUnaryPrefixOperator(t.type)) {
        lex->consumeToken();
        ExpressionAST *expr = parseUnaryExpression();
        if (!success) return nullptr;

        // optimization, if expr is a real literal, merge the actual value
        if (expr->ast_type == AST_LITERAL) {
            auto lit = (LiteralAST *)expr;
            switch (lit->typeAST->basic_type) {
            case BASIC_TYPE_FLOATING:
                if (t.type == TK_BANG) {
                    Error("The bang operator cannot be used with floating point numbers");
                    return nullptr;
                } else if (t.type == TK_MINUS) {
                    lit->_f64 = -lit->_f64;
                    return expr;
                } else {
                    assert(t.type == TK_PLUS);
                    // a plus unary sign can be ignored
                    return expr;
                }
                break;
            case BASIC_TYPE_BOOL:
                if (t.type == TK_BANG) {
                    lit->_bool = !lit->_bool;
                    return expr;
                } else {
                    Error("Operator %s cannot be used with boolean types", TokenTypeToStr(t.type));
                    return nullptr;
                }
                break;
            case BASIC_TYPE_STRING:
                Error("The type string does not support unary operators");
                return nullptr;
                break;
            case BASIC_TYPE_INTEGER:
                if (t.type == TK_BANG) {
                    Error("Operator ! cannot be used for integer types");
                    return nullptr;
                    //UnaryOperationAST *un = new UnaryOperationAST();
                    //setASTinfo(this, un);
                    //un->op = t.type;
                    //un->expr = expr;                    
                    //return un;
                } else if (t.type == TK_MINUS) {
                    if (lit->typeAST->isSigned) {
                        lit->_s64 = -lit->_s64;
                    } else {
                        lit->_s64 = -(s64)lit->_u64;
                        lit->typeAST = getType(TK_S64, nullptr);
                    }
                    return expr;
                } else {
                    assert(t.type == TK_PLUS);
                    // a plus unary sign can be ignored
                    return expr;
                }
                break;
            }
        }
        UnaryOperationAST *un = NEW_AST(UnaryOperationAST);
        un->op = t.type;
        un->expr = expr;
        return un;
    } else if (t.type == TK_RUN) {
        return parseRunDirective();
    }
    // @TODO: Handle postfix operators after the parseLiteral
    return parseLiteral();
}

ExpressionAST *Parser::parseBinOpExpressionRecursive(u32 oldprec, ExpressionAST *lhs)
{
    TOKEN_TYPE cur_type;

    while (1) {
        cur_type = lex->getTokenType();
        if (isBinOperator(cur_type)) {
            u32 cur_prec = getPrecedence(cur_type);
            if (cur_prec < oldprec) {
                return lhs;
            } else {
                lex->consumeToken();
                ExpressionAST *rhs = parseUnaryExpression();
                if (isBinOperator(lex->getTokenType())) {
                    u32 newprec = getPrecedence(lex->getTokenType());
                    if (cur_prec < newprec) {
                        rhs = parseBinOpExpressionRecursive(cur_prec + 1, rhs);
                    }
                } 
                BinaryOperationAST *bin = NEW_AST(BinaryOperationAST);
                bin->lhs = lhs;
                bin->rhs = rhs;
                bin->op = cur_type;
                lhs = bin;
            }
        } else {
            break;
        }
    }
    return lhs;
}

ExpressionAST *Parser::parseBinOpExpression()
{
    ExpressionAST *lhs = parseUnaryExpression();
    return parseBinOpExpressionRecursive( 0, lhs);
}

ExpressionAST * Parser::parseAssignmentOrExpression()
{
    ExpressionAST *lhs = parseBinOpExpression();
    TOKEN_TYPE type;

    type = lex->getTokenType();
    if (isAssignmentOperator(type)) {
        lex->consumeToken();
        AssignmentAST *assign = NEW_AST(AssignmentAST);
        assign->lhs = lhs;
        assign->op = type;
        assign->rhs = parseAssignmentOrExpression();
        return assign;
    } 
    return lhs;
}

ExpressionAST * Parser::parseExpression()
{
    return parseBinOpExpression();
}

void Parser::parseImportDirective()
{
    Token t;
    
    // @TODO: make import be module based with module folders
    // and appending extension, as well as figuring out libs to link against

    MustMatchToken(TK_IMPORT);
    if (!success) return;

    lex->getNextToken(t);
    if (t.type != TK_STRING) {
        Error("When parsing an #import directive, a string needs to follow\n");
        return;
    }

    // import directives can define new functions
    // as well as new types, all in global scope / Or the current scope?

    // things not allowed on an import
    /*
       - full function definition? (to be discussed)
       - #run directives ? 
       - #load directive
    */
    // things ONLY allowed on an import
    /*
    - #foreign
    - 
    */

    // this could be done in parallel if needed be
    Parser import_parser;
    import_parser.isImport = true;
    import_parser.interp = interp;
    import_parser.current_scope = current_scope;
    // All modules are in the modules folder, with the extension
    // One day this will be a search path
    char fname[64];
    sprintf_s(fname, "modules/%s.rad", t.string);
    bool val;
    // This does the equivalent of pragma once, and also
    // records the libraries we opened (#import)
    if (!top_level_ast->imports.get(t.string, val)) {
        top_level_ast->imports.put(t.string, true);
        import_parser.Parse(fname, pool, top_level_ast);

        if (!import_parser.success) {
            errorString = strcpy(errorString, import_parser.errorStringBuffer);
            success = false;
        }
    }
}

void Parser::parseLoadDirective()
{
    Token t;
    lex->getNextToken(t);

    MustMatchToken(TK_LOAD);
    if (!success) return;

    lex->getNextToken(t);
    if (t.type != TK_STRING) {
        Error("When parsing an #load directive, a string needs to follow\n");
        return;
    }

    // load directives can define new functions
    // as well as new types, all in global scope / Or the current scope?

    // this could be done in parallel if needed be
    Parser import_parser;
    import_parser.interp = interp;
    import_parser.current_scope = current_scope;
    import_parser.Parse(t.string, pool, top_level_ast);

    if (!import_parser.success) {
        errorString = strcpy(errorString, import_parser.errorStringBuffer);
        success = false;
    }
}

RunDirectiveAST* Parser::parseRunDirective()
{
    MustMatchToken(TK_RUN);
    
    // After run, it is an expression in general. Run can appear anywhere, and affect (or not)
    // the code and what is parsed. 
    // if run produces output, it should be inserted (on a separate compilation phase)
    ExpressionAST *expr;

    expr = parseUnaryExpression();
    if (!success) {
        return nullptr;
    }

    RunDirectiveAST *run = NEW_AST(RunDirectiveAST);
    run->expr = expr;

    // have a list of all run directives to process them later on
//    top_level_ast->run_items.push_back(run);
    return run;
}

DefinitionAST *Parser::parseDefinition()
{
    Token t;
    lex->lookaheadToken(t);
    if (t.type == TK_OPEN_PAREN) {
        bool oneArgument = (lex->checkAheadToken(TK_IDENTIFIER, 1) &&
            lex->checkAheadToken(TK_COLON, 2));
        bool noArguments = lex->checkAheadToken(TK_CLOSE_PAREN, 1);

        if (oneArgument || noArguments) {
            // if we encounter a [ ( IDENTIFER :  ] sequence, this is 
            // a function and not an expression, as we do not use the COLON
            // for anything else. When functions get more complex, this 
            // check will be too. 
            return parseFunctionDefinition();
        }
    } else if (t.type == TK_STRUCT) {
        return parseStructDefinition();
    }
    return parseExpression();
}

VariableDeclarationAST * Parser::parseDeclaration(bool isStruct)
{
    Token t;
    lex->getNextToken(t);
    VariableDeclarationAST *decl = NEW_AST(VariableDeclarationAST);

    if (t.type != TK_IDENTIFIER) {
        Error("Identifier expected but not found\n");
        return nullptr;
    }

    decl->varname = t.string;
    lex->lookaheadToken(t);

    if ((t.type != TK_COLON) &&
        (t.type != TK_DOUBLE_COLON) &&
        (t.type != TK_IMPLICIT_ASSIGN)) {
        Error("Declaration needs a colon, but found token: %s\n", TokenTypeToStr(t.type));
        return nullptr;
    }

    if (t.type == TK_COLON) {
        lex->consumeToken();
        // we are doing a variable declaration
        decl->specified_type = parseType();
        if (!success) {
            return nullptr;
        }
        lex->lookaheadToken(t);
    } 
    
    if (t.type == TK_DOUBLE_COLON) {
        decl->flags |= DECL_FLAG_IS_CONSTANT;
    }

    if ((t.type == TK_ASSIGN) || (t.type == TK_DOUBLE_COLON)
        || (t.type == TK_IMPLICIT_ASSIGN)) {
        lex->consumeToken();
        // we are doing an assignment or initial value
        decl->definition = parseDefinition();
        if (!success) {
            return nullptr;
        }
        if (decl->definition->ast_type == AST_FUNCTION_DEFINITION) {
            // we want to be able to find the name of the function
            auto fundef = (FunctionDefinitionAST *)decl->definition;
            fundef->var_decl = decl;
        } else if (decl->definition->ast_type == AST_STRUCT_DEFINITION) {
            // we need this pointer for C generation ordering
            auto sdef = (StructDefinitionAST *)decl->definition;
            sdef->struct_type->decl = decl;
        }
        lex->lookaheadToken(t);
    } else if (t.type != TK_SEMICOLON) {
        Error("Declaration is malformed, a semicolon was expected");
        if (!success) {
            return nullptr;
        }
    }
    if (!decl->definition || decl->definition->needsSemiColon) {
        // @TODO: support compiler flags and others here
        if (t.type != TK_SEMICOLON) {
            Error("Declaration needs to end with a semicolon\n");
            if (!success) {
                return nullptr;
            }
        }
        lex->getNextToken(t);
    }
    if (!isStruct) {
        // struct members do not follow the AddDeclaration to scope
        AddDeclarationToScope(decl);
    }
    return decl;
}

// first version, just return a list of AST
FileAST *Parser::Parse(const char *filename, PoolAllocator *pool, FileAST *fast)
{
	Lexer lex;
    this->lex = &lex;
    this->pool = pool;

    CPU_SAMPLE("Parser Main");

    lex.setPoolAllocator(pool);

    if (errorString == nullptr) {
        errorString = errorStringBuffer;
        errorString[0] = 0;
    }

    if (!lex.openFile(filename)) {
        errorString += sprintf(errorString, "Error: File [%s] could not be opened to be processed\n", filename);
        return nullptr;
    }

    return ParseInternal(fast);
}

FileAST * Parser::ParseFromString(const char *str, u64 str_size, PoolAllocator *pool, FileAST *fast)
{
    Lexer lex;
    this->lex = &lex;
    this->pool = pool;

    CPU_SAMPLE("Parser From String");

    lex.setPoolAllocator(pool);

    if (errorString == nullptr) {
        errorString = errorStringBuffer;
        errorString[0] = 0;
    }

    if (!lex.loadString(str, str_size)) {
        errorString += sprintf(errorString, "Error: String could not be loaded to be processed\n");
        return nullptr;
    }

    return ParseInternal(fast);
}

FileAST * Parser::ParseInternal(FileAST *fast)
{
    FileAST *file_inst = nullptr;

    if (fast != nullptr) {
        file_inst = fast;
    } else {
        file_inst = new (pool) FileAST;
        file_inst->global_scope.parent = nullptr;
        file_inst->scope = &file_inst->global_scope;            
        file_inst->filename = CreateTextType(pool, lex->getFileData()->getFilename());
    }
     
    top_level_ast = file_inst;
    success = true;

    if (errorString == nullptr) {
        errorString = errorStringBuffer;
    }

    defineBuiltInTypes();

    lex->parseFile();
    interp->files.push_back(lex->getFileData());

    if (option_printTokens) {
        while (!lex->checkToken(TK_LAST_TOKEN)) {
            Token t;
            lex->getNextToken(t);
            t.print();
        }
        lex->setTokenStreamPosition(0);
    }

    if (current_scope == nullptr) {
        current_scope = &file_inst->global_scope;        
    }

    while (!lex->checkToken(TK_LAST_TOKEN)) {
        Token t;
        lex->lookaheadToken(t);
        if (t.type == TK_IMPORT) {
            parseImportDirective();
        } else if (t.type == TK_LOAD) {
            parseLoadDirective();
        } else if (t.type == TK_RUN) {
            RunDirectiveAST *r = parseRunDirective();
            if (!success) {
                return nullptr;
            }
            // Allow semicolons after a #run directive
            lex->lookaheadToken(t);
            if (t.type == TK_SEMICOLON) lex->consumeToken();

            file_inst->items.push_back(r);
        } else {
            VariableDeclarationAST *d = parseDeclaration();
            
            if (!success) {
                return nullptr;
            }
            
            file_inst->items.push_back(d);
        }
    }

    this->lex = nullptr;
    return file_inst;
}


