#include "Parser.h"
#include "Lexer.h"
#include "PoolAllocator.h"
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

void traverseAST(FileAST *root);
u32 process_scope_variables(Scope *scope);
bool infer_types(VariableDeclarationAST *decl);
TypeAST *deduceType(ExpressionAST *expr);

extern bool option_printTokens;

#define NEW_AST(ast_type) (ast_type *) setASTinfo(this, (BaseAST *) new(this->pool) ast_type )

static u64 sequence_id = 100;

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

void Parser::Error(const char *msg, ...)
{
    va_list args;
    SrcLocation loc;
    lex->getLocation(loc);
    u32 off = sprintf_s(errorString, "%s:%d:%d: error : ", lex->getFilename(), 
        loc.line, loc.col);

    va_start(args, msg);
    vsprintf_s(errorString + off, sizeof(errorString) - off, msg, args);
    va_end(args);
    success = false;
}

bool Parser::MustMatchToken(TOKEN_TYPE type, const char *msg)
{
    if (!lex->checkToken(type)) {
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
        if (!strcmp(d->varname, decl->varname)) {
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
    for (auto d : struct_def->struct_type.struct_scope.decls) {
        if (!strcmp(d->varname, decl->varname)) {
            Error("Variable %s is already defined within this struct", decl->varname);
            return false;
        }
    }
    struct_def->struct_type.struct_scope.decls.push_back(decl);
    decl->scope = &struct_def->struct_type.struct_scope;
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
        || (type == TK_BANG);
}

static bool isAssignmentOperator(TOKEN_TYPE type)
{
    return (type == TK_ASSIGN)
        || (type == TK_MUL_ASSIGN)
        || (type == TK_DIV_ASSIGN)
        || (type == TK_MOD_ASSIGN)
        || (type == TK_ADD_ASSIGN)
        || (type == TK_SUB_ASSIGN)
        || (type == TK_LEFT_ASSIGN)
        || (type == TK_RIGHT_ASSIGN)
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
        || (type == TK_RSHIFT)
        || (type == TK_LSHIFT)
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
    case TK_LSHIFT:
    case TK_RSHIFT:
        return 3;
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

static bool isVariableTypeToken(TOKEN_TYPE t)
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
        || (t == TK_STRING_KEYWORD);
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

DirectTypeAST *Parser::createType(TOKEN_TYPE tktype, TextType name)
{
    DirectTypeAST *type = NEW_AST(DirectTypeAST);
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
    case TK_INT:
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
    case TK_S64:
        type->basic_type = BASIC_TYPE_INTEGER;
        type->isSigned = true;
        type->size_in_bytes = 8;
        break;
    case TK_F32:
        type->basic_type = BASIC_TYPE_FLOATING;
        type->size_in_bytes = 4;
        break;
    case TK_FLOAT:
    case TK_F64:
        type->basic_type = BASIC_TYPE_FLOATING;
        type->size_in_bytes = 8;
        break;
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
    lex->lookaheadToken(t);
    if ((t.type != TK_IDENTIFIER) && !isVariableTypeToken(t.type)) {
        Error("Variable type token could not be found, but we found: %s\n",
            TokenTypeToStr(t.type));
        return nullptr;
    }
    lex->consumeToken();

    return createType(t.type, t.string);
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
        fundec->return_type = createType(TK_VOID, "void");
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
            MustMatchToken(TK_SEMICOLON, "Statement needs to end in semicolon");
            if (!success) {
                return nullptr;
            }
        } else {
            statement = parseAssignmentOrExpression();
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
        if (lex->checkToken(TK_COMMA)) {
            lex->consumeToken();
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
    assert(t.type == TK_IDENTIFIER);
    lex->consumeToken(); // this consumes the identifier

    if (lex->checkAheadToken(TK_PERIOD, 0)) {
        // compound statement
        VarReferenceAST *comp = NEW_AST(VarReferenceAST);
        comp->name = t.string;
        lex->consumeToken();
        if (!lex->checkAheadToken(TK_IDENTIFIER, 0)) {
            Error("An identifier must follow after a period access expression");
            return nullptr;
        }
        comp->next = parseVarReference();
        if (comp->next) {
            comp->next->prev = comp;
            // the scope for a variable reference is that of the enclosing struct
            // since we do not yet know it, just null it to be safe
            comp->next->scope = nullptr;
        }
        return comp;
    } else if (lex->checkAheadToken(TK_OPEN_SQBRACKET, 0)) {
        // This is an array access
        assert(!"Array access in expressions is not implemented yet");
        return nullptr;
    } else {

        if (t.type != TK_IDENTIFIER) {
            Error("An identifier must follow after a period access expression");
            return nullptr;
        }

        IdentifierAST *ex = NEW_AST(IdentifierAST);
        ex->name = t.string;
        return ex;
    }
}

ExpressionAST * Parser::parseLiteral()
{
    Token t;
    lex->getCurrentToken(t);

    if (t.type == TK_IDENTIFIER) {
        return parseVarReference();
    } else if ((t.type == TK_NUMBER) || (t.type == TK_TRUE) ||
        (t.type == TK_FNUMBER) || (t.type == TK_STRING) || (t.type == TK_FALSE)) {
        auto ex = NEW_AST(LiteralAST);
        setASTinfo(this, &ex->typeAST);
        lex->consumeToken();

        ex->typeAST.isLiteral = true;

        if (t.type == TK_NUMBER) {
            ex->typeAST.basic_type = BASIC_TYPE_INTEGER;
            ex->typeAST.size_in_bytes = 8;
            ex->_u64 = t._u64;
        } else if (t.type == TK_FNUMBER) {
            ex->typeAST.basic_type = BASIC_TYPE_FLOATING;
            ex->typeAST.size_in_bytes = 8;
            ex->_f64 = t._f64;
        } else if (t.type == TK_STRING) {
            ex->typeAST.basic_type = BASIC_TYPE_STRING;
            ex->typeAST.size_in_bytes = 8+8;
            ex->str = t.string;
        } else if (t.type == TK_TRUE) {
            ex->typeAST.basic_type = BASIC_TYPE_BOOL;
            ex->typeAST.size_in_bytes = 1;
            ex->_bool = true;
        } else if (t.type == TK_FALSE) {
            ex->typeAST.basic_type = BASIC_TYPE_BOOL;
            ex->typeAST.size_in_bytes = 1;
            ex->_bool = false;
        }
        return ex;
    } else if (t.type == TK_OPEN_PAREN) {
        SrcLocation loc;
        loc = t.loc;
        lex->consumeToken();

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
            lex->consumeToken();
            lex->getNextToken(t);

            if (isHexNumber(t)) {
                Error("After a period we need to see normal numbers, not hex numbers");
                return nullptr;
            }

            auto ex = NEW_AST(LiteralAST);
            setASTinfo(this, &ex->typeAST);

            ex->typeAST.isLiteral = true;
            ex->typeAST.basic_type = BASIC_TYPE_FLOATING;
            ex->typeAST.size_in_bytes = 8;

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
    } else if (isUnaryPrefixOperator(t.type)) {
        lex->consumeToken();
        ExpressionAST *expr = parseUnaryExpression();
        if (!success) return nullptr;

        // optimization, if expr is a real literal, merge the actual value
        if (expr->ast_type == AST_LITERAL) {
            auto lit = (LiteralAST *)expr;
            switch (lit->typeAST.basic_type) {
            case BASIC_TYPE_FLOATING:
                if (t.type == TK_BANG) {
                    Error("The bang operator cannot be used with floating point numbers");
                    return nullptr;
                } else if (t.type == TK_MINUS) {
                    lit->_f64 = -lit->_f64;
                    lit->typeAST.isSigned = true;
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
                    if (lit->typeAST.isSigned) {
                        lit->_s64 = -lit->_s64;
                    } else {
                        lit->_s64 = -(s64)lit->_u64;
                        lit->typeAST.isSigned = true;
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
    import_parser.current_scope = current_scope;
    // All modules are in the modules folder, with the extension
    // One day this will be a search path
    char fname[64];
    sprintf_s(fname, "modules/%s.jai", t.string);
    import_parser.Parse(fname, pool, top_level_ast);

    if (!import_parser.success) {
        strncpy_s(errorString, import_parser.errorString, sizeof(errorString));
        success = false;
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
    import_parser.current_scope = current_scope;
    import_parser.Parse(t.string, pool, top_level_ast);

    if (!import_parser.success) {
        strncpy_s(errorString, import_parser.errorString, sizeof(errorString));
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

    // TODO: we might want to have a list of all run directives to process them later on

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
            sdef->struct_type.decl = decl;
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
    FileAST *file_inst = nullptr;

    lex.setPoolAllocator(pool);

    if (!lex.openFile(filename)) {
        sprintf_s(errorString, "Error: File [%s] could not be opened to be processed\n", filename);
        return nullptr;
    }

    if (fast != nullptr) {
        file_inst = fast;
    } else {
        file_inst = new (pool) FileAST;
        file_inst->global_scope.parent = nullptr;
    }
     
    top_level_ast = file_inst;
    success = true;

    lex.parseFile();

    if (option_printTokens) {
        while (!lex.checkToken(TK_LAST_TOKEN)) {
            Token t;
            lex.getNextToken(t);
            t.print();
        }
        lex.setTokenStreamPosition(0);
    }

    if (current_scope == nullptr) {
        current_scope = &file_inst->global_scope;        
    }

	while (!lex.checkToken(TK_LAST_TOKEN)) {
        Token t;
        lex.lookaheadToken(t);
        if (t.type == TK_IMPORT) {
            parseImportDirective();
        } else if (t.type == TK_LOAD) {
            parseLoadDirective();
        } else if (t.type == TK_RUN) {
            RunDirectiveAST *r = parseRunDirective();
            if (!success) {
                return nullptr;
            }

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
