#include "Parser.h"
#include "Lexer.h"
#include "PoolAllocator.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void Parser::Error(const char *msg)
{
	SrcLocation loc;
	lex->getLocation(loc);
	printf("Error %s:%lld - %s", lex->getFilename(), loc.line, msg);
	exit(1);
}

void Parser::MustMatchToken(TOKEN_TYPE type, char *msg)
{
    if (!lex->checkToken(type)) {
        char err[128] = {};
        u32 offset = 0;
        if (msg) {
            offset = snprintf(err, sizeof(err), "%s\n    ", msg);
        }
        snprintf(err + offset, sizeof(err) - offset, "Token %s was expected, but we found: %s\n", 
            TokenTypeToStr(type), TokenTypeToStr(lex->getTokenType()));
        Error(err);
    }
    lex->consumeToken();
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

TypeAST *Parser::parseDirectType()
{
    // @TODO: support pointer, arrays, etc
    Token t;
    lex->lookaheadToken(t);
    MustMatchToken(TK_IDENTIFIER);

    DirectTypeAST *type = new DirectTypeAST();
    type->name = t.string;
    if (!strcmp(t.string, "string")) {
        type->isString = true;
    } else if (!strcmp(t.string, "int")) {
        type->type = I64;
    } else if (!strcmp(t.string, "float")) {
        type->type = F32;
    } else if (!strcmp(t.string, "i8")) {
        type->type = I8;
    } else if (!strcmp(t.string, "i16")) {
        type->type = I16;
    } else if (!strcmp(t.string, "i32")) {
        type->type = I32;
    } else if (!strcmp(t.string, "i64")) {
        type->type = I64;
    } else if (!strcmp(t.string, "u8")) {
        type->type = U8;
    } else if (!strcmp(t.string, "u16")) {
        type->type = U16;
    } else if (!strcmp(t.string, "u32")) {
        type->type = U32;
    } else if (!strcmp(t.string, "u64")) {
        type->type = U64;
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
    MustMatchToken(TK_IDENTIFIER, "Argument declaration needs to start with an identifier");
    arg->name = t.string;

    MustMatchToken(TK_COLON, "Argument declaration needs a colon between identifier and type");

    arg->type = parseType();
    return arg;
}

FunctionDeclarationAST *Parser::parseFunctionDeclaration()
{
    MustMatchToken(TK_OPEN_PAREN, "Function declarations need a parenthesis");

    FunctionDeclarationAST *fundec = new FunctionDeclarationAST();
    while (!lex->checkToken(TK_CLOSE_PAREN)) {
        ArgumentDeclarationAST *arg = Parser::parseArgumentDeclaration();
        fundec->arguments.push_back(arg);
        if (lex->checkToken(TK_COMMA)) {
            lex->consumeToken();
        } else if (!lex->checkToken(TK_CLOSE_PAREN)) {
            Error("Comma must be used in between parameters in a function\n");
        }
    }
    lex->consumeToken();
    // Now do the return value, which can be empty
    if (lex->checkToken(TK_RETURN_ARROW)) {
        lex->consumeToken();
        fundec->return_type = parseType();
    }
    return fundec;
}

ReturnStatementAST *Parser::parseReturnStatement()
{
    ReturnStatementAST *ret = new ReturnStatementAST();
    MustMatchToken(TK_RETURN);

    ret->ret = parseExpression();
    MustMatchToken(TK_SEMICOLON, "Statement needs to end in semicolon");
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
        } else {
            statement = parseExpression();
            MustMatchToken(TK_SEMICOLON, "Statement needs to end in semicolon");
        }
        return statement;
    } else if (cur_type == TK_OPEN_BRACKET) {
        return parseStatementBlock();
    } else if (cur_type == TK_RETURN) {
        return parseReturnStatement();
    } else {
        return parseExpression();
    }

}

StatementBlockAST *Parser::parseStatementBlock()
{
    if (!lex->checkToken(TK_OPEN_BRACKET)) {
        Error("Was expecting an open bracket\n");
    }
    lex->consumeToken(); // consume the {
    StatementBlockAST *block = new StatementBlockAST();
    while (!lex->checkToken(TK_CLOSE_BRACKET)) {
        StatementAST *statement = nullptr;
        statement = parseStatement();
        block->statements.push_back(statement);
        if (lex->checkToken(TK_LAST_TOKEN)) {
            Error("Failed to find a matching close bracket");
        }
    }
    lex->consumeToken(); // match }

    return block;
}


FunctionDefinitionAST *Parser::parseFunctionDefinition()
{
    FunctionDefinitionAST *fundef = new FunctionDefinitionAST();

    fundef->declaration = parseFunctionDeclaration();
    fundef->function_body = parseStatementBlock();
    return fundef;
}

FunctionCallAST * Parser::parseFunctionCall()
{
    FunctionCallAST *funcall = new FunctionCallAST();
    Token t;
    lex->getNextToken(t);

    assert(t.type == TK_IDENTIFIER);
    funcall->function_name = t.string;

    MustMatchToken(TK_OPEN_PAREN);
    while (!lex->checkToken(TK_CLOSE_PAREN)) {
        ExprAST * expr = parseExpression();
        funcall->args.push_back(expr);
        if (lex->checkToken(TK_COMMA)) {
            lex->consumeToken();
        } 
    }

    MustMatchToken(TK_CLOSE_PAREN);

    return funcall;
}

ExprAST * Parser::parseCastExpression()
{
    Token t;
    lex->getNextToken(t);

    if (t.type == TK_IDENTIFIER) {
        IdentAST *ex = new IdentAST();
        ex->name = t.string;
        return ex;
    } else if ((t.type == TK_NUMBER) || (t.type == TK_FNUMBER)) {
        ConstNumAST *ex = new ConstNumAST();
        if (t.type == TK_NUMBER) {
            ex->type = U64;
            ex->pl.pu64 = t.pl.pu64;
        } else {
            ex->type = F64;
            ex->pl.pf64 = t.pl.pf64;
        }
        return ex;
    } else if ((t.type == TK_STRING)) {
        ConstStringAST *str = new ConstStringAST();
        str->str = t.string;
        return str;
    }
    //@TODO: have an error message here
    printf("Could not parse a expression!");
    return nullptr;
}

ExprAST *Parser::parseBinOpExpressionRecursive(u32 oldprec, ExprAST *lhs)
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
                ExprAST *rhs = parseCastExpression();
                if (isBinOperator(lex->getTokenType())) {
                    u32 newprec = getPrecedence(lex->getTokenType());
                    if (cur_prec < newprec) {
                        rhs = parseBinOpExpressionRecursive(cur_prec + 1, rhs);
                    }
                } 
                BinOpAST *bin = new BinOpAST();
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

ExprAST *Parser::parseBinOpExpression()
{
    ExprAST *lhs = parseCastExpression();
    return parseBinOpExpressionRecursive( 0, lhs);
}

ExprAST * Parser::parseAssignmentExpression()
{
    ExprAST *lhs = parseBinOpExpression();
    TOKEN_TYPE type;

    type = lex->getTokenType();
    if (isAssignmentOperator(type)) {
        // @TODO : we would need to have here a RHS evaluation
        AssignAST *assign = new AssignAST();
        assign->lhs = lhs;
        assign->op = type;
        assign->rhs = parseAssignmentExpression();
        return assign;
    } 
    return lhs;
}

ExprAST * Parser::parseExpression()
{
    Token t;
    lex->lookaheadToken(t);
    if (t.type == TK_OPEN_PAREN) {
        lex->getNextToken(t);
        ExprAST *expr = parseExpression();
        lex->getNextToken(t);
        if (t.type != TK_CLOSE_PAREN) {
            Error("Cound not find a matching close parentesis\n");
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
        // ok, this could be an expression or a function definition!
        // hard to tell, so we will try both and see which one produces a result
        u32 lex_pos = lex->getTokenStreamPosition();
        FunctionDefinitionAST *func_def = parseFunctionDefinition();
        if (func_def != nullptr) {
            return func_def;
        }

        // @TODO: use lookahead instead of token rewind
        // if we are here, means that we could not parse the function definition
        // it has to be an expression, but we need to reset the parsing stream
        lex->setTokenStreamPosition(lex_pos);
        // fall through to the normal expression parsing
    }
    return parseExpression();
}

DeclAST * Parser::parseDeclaration()
{
    Token t;
    lex->getNextToken(t);
    DeclAST *decl = new DeclAST();

    if (t.type != TK_IDENTIFIER) {
        Error("Identifier expected but not found\n");
    }

    decl->varname = t.string;
    lex->lookaheadToken(t);
    if (t.type == TK_COLON) {
        lex->consumeToken();
        // we are doing a variable declaration
        decl->specified_type = parseType();
        lex->lookaheadToken(t);
    } 
    
    if (t.type == TK_DOUBLE_COLON) {
        decl->is_constant = true;
    }

    if ((t.type == TK_ASSIGN) || (t.type == TK_DOUBLE_COLON)
        || (t.type == TK_IMPLICIT_ASSIGN)) {
        lex->consumeToken();
        // we are doing an assignment or initial value
        decl->definition = parseDefinition();
        lex->lookaheadToken(t);
    } else if (t.type != TK_SEMICOLON) {
        Error("Declaration is malformed");
    }
    if (!decl->definition || decl->definition->needsSemiColon()) {
        // @TODO: support compiler flags and others here
        if (t.type != TK_SEMICOLON) {
            Error("Declaration needs to end with a semicolon\n");
        }
        lex->getNextToken(t);
    }
    return decl;
}

// first version, just return a list of AST
Array<BaseAST *> *Parser::Parse(const char *filename, PoolAllocator *pool)
{
	Lexer lex;
    this->lex = &lex;
    Array<BaseAST *> *vec = new Array<BaseAST *>(20);

    lex.setPoolAllocator(pool);

	lex.openFile(filename);
    lex.parseFile();

	while (!lex.checkToken(TK_LAST_TOKEN)) {
		// we got a token, figure out what AST to build
        DeclAST *d = parseDeclaration();
        vec->push_back(d);
                
        //lex->getNextToken(t);
        //t.print();
	}

    this->lex = nullptr;
	return vec;
}