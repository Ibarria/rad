#include "Parser.h"
#include "Lexer.h"
#include "PoolAllocator.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#ifndef WIN32
# define sprintf_s sprintf
# define vsprintf_s vsnprintf
#endif

void traverseAST(FileAST *root);
u32 process_scope_variables(Scope *scope);
bool infer_types(VariableDeclarationAST *decl);
TypeAST *deduceType(ExpressionAST *expr);

extern bool option_printTokens;


static void setASTloc(Parser *p, BaseAST *ast)
{
    SrcLocation loc;
    p->lex->getLocation(loc);
    ast->line_num = loc.line;
    ast->char_num = loc.col;
    ast->filename = p->lex->getFilename();
}

static void setASTinfo(Parser *p, BaseAST *ast)
{
    setASTloc(p, ast);
    ast->scope = p->current_scope;
}

void Parser::Error(const char *msg, ...)
{
    va_list args;
    SrcLocation loc;
    lex->getLocation(loc);
    u32 off = sprintf_s(errorString, "%s(%d): error : ", lex->getFilename(), loc.line);

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
    return true;
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
        || (t == TK_STRING);
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

    DirectTypeAST *type = new DirectTypeAST();
    setASTinfo(this, type);
    type->name = t.string;
    switch (t.type) {
    case TK_STRING:
        type->type = BASIC_TYPE_STRING;
        break;
    case TK_U8:
        type->type = BASIC_TYPE_U8;
        break;
    case TK_U16:
        type->type = BASIC_TYPE_U16;
        break;
    case TK_U32:
        type->type = BASIC_TYPE_U32;
        break;
    case TK_INT:
    case TK_U64:
        type->type = BASIC_TYPE_U64;
        break;
    case TK_S8:
        type->type = BASIC_TYPE_S8;
        break;
    case TK_S16:
        type->type = BASIC_TYPE_S16;
        break;
    case TK_S32:
        type->type = BASIC_TYPE_S32;
        break;
    case TK_S64:
        type->type = BASIC_TYPE_S64;
        break;
    case TK_F32:
        type->type = BASIC_TYPE_F32;
        break;
    case TK_FLOAT:
    case TK_F64:
        type->type = BASIC_TYPE_F64;
        break;
    case TK_IDENTIFIER :
        assert(!"Identifier types, custom types are not implemented");
    }

    // @TODO: support custom types
    return type;
}

TypeAST * Parser::parseType()
{
    if (lex->checkToken(TK_OPEN_PAREN)) {
        return parseFunctionDeclaration();
    }

    return parseDirectType();
}

ArgumentDeclarationAST *Parser::parseArgumentDeclaration()
{
    Token t;
    lex->lookaheadToken(t);
    ArgumentDeclarationAST *arg = new ArgumentDeclarationAST();
    setASTinfo(this, arg);
    MustMatchToken(TK_IDENTIFIER, "Argument declaration needs to start with an identifier");
    if (!success) {
        delete arg;
        return nullptr;
    }
    arg->name = t.string;

    MustMatchToken(TK_COLON, "Argument declaration needs a colon between identifier and type");

    if (!success) {
        delete arg;
        return nullptr;
    }

    arg->type = parseType();

    if (!success) {
        delete arg;
        return nullptr;
    }
    return arg;
}

FunctionTypeAST *Parser::parseFunctionDeclaration()
{
    MustMatchToken(TK_OPEN_PAREN, "Function declarations need to start with an open parenthesis");
    if (!success) {
        return nullptr;
    }

    FunctionTypeAST *fundec = new FunctionTypeAST();
    setASTinfo(this, fundec);

    while (!lex->checkToken(TK_CLOSE_PAREN)) {
        ArgumentDeclarationAST *arg = Parser::parseArgumentDeclaration();
        if (!success) {
            delete fundec;
            return nullptr;
        }

        fundec->arguments.push_back(arg);
        if (lex->checkToken(TK_COMMA)) {
            lex->consumeToken();
        } else if (!lex->checkToken(TK_CLOSE_PAREN)) {
            Error("Comma must be used in between parameters in a function\n");
            delete fundec;
            return nullptr;
        }
    }
    lex->consumeToken();
    // Now do the return value, which can be empty
    if (lex->checkToken(TK_RETURN_ARROW)) {
        lex->consumeToken();
        fundec->return_type = parseType();
    }
    if (!success) {
        delete fundec;
        return nullptr;
    }
    return fundec;
}

ReturnStatementAST *Parser::parseReturnStatement()
{
    ReturnStatementAST *ret = new ReturnStatementAST();
    setASTinfo(this, ret);
    MustMatchToken(TK_RETURN);
    if (!success) {
        delete ret;
        return nullptr;
    }

    ret->ret = parseExpression();
    if (!success) {
        delete ret;
        return nullptr;
    }
    MustMatchToken(TK_SEMICOLON, "Statement needs to end in semicolon");
    if (!success) {
        delete ret;
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
            statement = parseExpression();
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
        statement =  parseExpression();
        MustMatchToken(TK_SEMICOLON, "Statement needs to end in semicolon");
        if (!success) {
            return nullptr;
        }
        return statement;
    }
}

StatementBlockAST *Parser::parseStatementBlock()
{
    if (!lex->checkToken(TK_OPEN_BRACKET)) {
        Error("We are trying to parse a statement block and it needs to start with an open bracket\n");
        return nullptr;
    }
    lex->consumeToken(); // consume the {
    StatementBlockAST *block = new StatementBlockAST();
    setASTinfo(this, block);
    // push scope
    block->scope.parent = current_scope;
    current_scope = &block->scope;

    while (!lex->checkToken(TK_CLOSE_BRACKET)) {
        StatementAST *statement = nullptr;
        statement = parseStatement();
        if (!success) {
            delete block;
            return nullptr;
        }
        block->statements.push_back(statement);
        if (lex->checkToken(TK_LAST_TOKEN)) {
            Error("Failed to find a matching close bracket, open bracket at line %d\n", block->line_num);
            if (!success) {
                delete block;
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
    FunctionDefinitionAST *fundef = new FunctionDefinitionAST();
    setASTinfo(this, fundef);

    fundef->declaration = parseFunctionDeclaration();
    if (!success) {
        delete fundef;
        return nullptr;
    }
    fundef->function_body = parseStatementBlock();
    if (!success) {
        delete fundef;
        return nullptr;
    }
    return fundef;
}

FunctionCallAST * Parser::parseFunctionCall()
{
    FunctionCallAST *funcall = new FunctionCallAST();
    Token t;
    setASTinfo(this, funcall);
    lex->getNextToken(t);

    assert(t.type == TK_IDENTIFIER);
    funcall->function_name = t.string;

    MustMatchToken(TK_OPEN_PAREN);
    if (!success) {
        delete funcall;
        return nullptr;
    }
    while (!lex->checkToken(TK_CLOSE_PAREN)) {
        ExpressionAST * expr = parseExpression();
        if (!success) {
            delete funcall;
            return nullptr;
        }
        funcall->args.push_back(expr);
        if (lex->checkToken(TK_COMMA)) {
            lex->consumeToken();
        } 
    }

    MustMatchToken(TK_CLOSE_PAREN);
    if (!success) {
        delete funcall;
        return nullptr;
    }

    return funcall;
}

ExpressionAST * Parser::parseLiteral()
{
    Token t;
    lex->getNextToken(t);

    if (t.type == TK_IDENTIFIER) {
        IdentifierAST *ex = new IdentifierAST();
        setASTinfo(this, ex);
        ex->name = t.string;
        return ex;
    } else if ((t.type == TK_NUMBER) || (t.type == TK_FNUMBER)) {
        ConstantNumberAST *ex = new ConstantNumberAST();
        setASTinfo(this, ex);

        if (t.type == TK_NUMBER) {
            ex->type = BASIC_TYPE_U64;
            ex->pl.pu64 = t.pl.pu64;
        } else {
            ex->type = BASIC_TYPE_F64;
            ex->pl.pf64 = t.pl.pf64;
        }
        return ex;
    } else if (t.type == TK_STRING) {
        ConstantStringAST *str = new ConstantStringAST();
        setASTinfo(this, str);

        str->str = t.string;
        return str;
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
    } 
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
                BinaryOperationAST *bin = new BinaryOperationAST();
                setASTinfo(this, bin);
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

ExpressionAST * Parser::parseAssignmentExpression()
{
    ExpressionAST *lhs = parseBinOpExpression();
    TOKEN_TYPE type;

    type = lex->getTokenType();
    if (isAssignmentOperator(type)) {
        lex->consumeToken();
        AssignmentAST *assign = new AssignmentAST();
        setASTinfo(this, assign);
        assign->lhs = lhs;
        assign->op = type;
        assign->rhs = parseAssignmentExpression();
        return assign;
    } 
    return lhs;
}

ExpressionAST * Parser::parseExpression()
{
    Token t;
    lex->lookaheadToken(t);
    if (t.type == TK_OPEN_PAREN) {
        SrcLocation loc;
        lex->getLocation(loc);
        lex->getNextToken(t);
        
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
    }
    return parseAssignmentExpression();
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
    }
    return parseExpression();
}

VariableDeclarationAST * Parser::parseDeclaration()
{
    Token t;
    lex->getNextToken(t);
    VariableDeclarationAST *decl = new VariableDeclarationAST();
    setASTinfo(this, decl);

    if (t.type != TK_IDENTIFIER) {
        Error("Identifier expected but not found\n");
        delete decl;
        return nullptr;
    }

    decl->varname = t.string;
    lex->lookaheadToken(t);

    if ((t.type != TK_COLON) &&
        (t.type != TK_DOUBLE_COLON) &&
        (t.type != TK_IMPLICIT_ASSIGN)) {
        Error("Declaration needs a colon, but found token: %s\n", TokenTypeToStr(t.type));
        delete decl;
        return nullptr;
    }

    if (t.type == TK_COLON) {
        lex->consumeToken();
        // we are doing a variable declaration
        decl->specified_type = parseType();
        if (!success) {
            delete decl;
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
            delete decl;
            return nullptr;
        }
        lex->lookaheadToken(t);
    } else if (t.type != TK_SEMICOLON) {
        Error("Declaration is malformed");
        if (!success) {
            delete decl;
            return nullptr;
        }
    }
    if (!decl->definition || decl->definition->needsSemiColon()) {
        // @TODO: support compiler flags and others here
        if (t.type != TK_SEMICOLON) {
            Error("Declaration needs to end with a semicolon\n");
            if (!success) {
                delete decl;
                return nullptr;
            }
        }
        lex->getNextToken(t);
    }
    AddDeclarationToScope(decl);
    return decl;
}

// first version, just return a list of AST
FileAST *Parser::Parse(const char *filename, PoolAllocator *pool)
{
	Lexer lex;
    this->lex = &lex;
    FileAST *file_inst = new FileAST();
    success = true;

    lex.setPoolAllocator(pool);

    if (!lex.openFile(filename)) {
        sprintf_s(errorString, "Error: File [%s] could not be opened to be compiled\n", filename);
        return nullptr;
    }
    lex.parseFile();

    if (option_printTokens) {
        while (!lex.checkToken(TK_LAST_TOKEN)) {
            Token t;
            lex.getNextToken(t);
            t.print();
        }
    }


    current_scope = &file_inst->scope;
    current_scope->parent = nullptr;

	while (!lex.checkToken(TK_LAST_TOKEN)) {
		// we got a token, figure out what AST to build
        VariableDeclarationAST *d = parseDeclaration();
        file_inst->items.push_back(d);

        if (!success) {
            return nullptr;
        }
	}


    this->lex = nullptr;
	return file_inst;
}
