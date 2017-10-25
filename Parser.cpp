#include "Parser.h"
#include "Lexer.h"
#include "PoolAllocator.h"

FunctionDeclarationAST *parseFunctionDeclaration(Lexer &lex);
FunctionDefinitionAST *parseFunctionDefinition(Lexer &lex);
DeclAST * parseDeclaration(Lexer &lex);
ExprAST * parseAssignmentExpression(Lexer & lex);
ExprAST * parseExpression(Lexer &lex);
StatementBlockAST *parseStatementBlock(Lexer &lex);

static void Error(const Lexer &lex, const char *msg)
{
	SrcLocation loc;
	lex.getLocation(loc);
	printf("Error %s:%lld - %s", lex.getFilename(), loc.line, msg);
	exit(1);
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

TypeAST *parseDirectType(Lexer &lex)
{
    // @TODO: support pointer, arrays, etc
    Token t;
    lex.getNextToken(t);
    if (t.type != TK_IDENTIFIER) {
        Error(lex, "Type can only be an identifier, for now\n");
    }
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

TypeAST * parseType(Lexer &lex)
{
    if (lex.checkToken(TK_OPEN_PAREN)) {
        return parseFunctionDeclaration(lex);
    }

    return parseDirectType(lex);
}

ArgumentDeclarationAST *parseArgumentDeclaration(Lexer &lex)
{
    Token t;
    lex.getNextToken(t);
    ArgumentDeclarationAST *arg = new ArgumentDeclarationAST();
    if (t.type != TK_IDENTIFIER) {
        Error(lex, "Argument declaration needs to start with an identifier\n");
    }
    arg->name = t.string;

    if (!lex.checkToken(TK_COLON)) {
        Error(lex, "Argument declaration needs a colon between identifier and type\n");
    }
    lex.consumeToken();

    arg->type = parseType(lex);
    return arg;
}

FunctionDeclarationAST *parseFunctionDeclaration(Lexer &lex)
{
    if (!lex.checkToken(TK_OPEN_PAREN)) {
        Error(lex, "Function declarations need a parenthesis\n");
    }
    lex.consumeToken();
    FunctionDeclarationAST *fundec = new FunctionDeclarationAST();
    while (!lex.checkToken(TK_CLOSE_PAREN)) {
        ArgumentDeclarationAST *arg = parseArgumentDeclaration(lex);
        fundec->arguments.push_back(arg);
        if (lex.checkToken(TK_COMMA)) {
            lex.consumeToken();
        } else if (!lex.checkToken(TK_CLOSE_PAREN)) {
            Error(lex, "Comma must be used in between parameters in a function\n");
        }
    }
    lex.consumeToken();
    // Now do the return value, which can be empty
    if (lex.checkToken(TK_RETURN_ARROW)) {
        lex.consumeToken();
        fundec->return_type = parseType(lex);
    }
    return fundec;
}

ReturnStatementAST *parseReturnStatement(Lexer &lex)
{
    ReturnStatementAST *ret = new ReturnStatementAST();
    if (!lex.checkToken(TK_RETURN)) {
        Error(lex, "A return statement needs the 'return' keyword at the start\n");
    }
    lex.consumeToken();

    ret->ret = parseExpression(lex);
    if (!lex.checkToken(TK_SEMICOLON)) {
        Error(lex, "Statement needs to end in semicolon\n");
    }
    lex.consumeToken();
    return ret;
}

StatementAST *parseStatement(Lexer &lex)
{
    TOKEN_TYPE cur_type;
    StatementAST *statement = nullptr;

    cur_type = lex.getTokenType();
    if (cur_type == TK_IDENTIFIER) {
        // could be a variable definition or a statement
        if (lex.checkAheadToken(TK_COLON,1)
            || lex.checkAheadToken(TK_DOUBLE_COLON, 1)
            || lex.checkAheadToken(TK_IMPLICIT_ASSIGN, 1)) {
            statement = parseDeclaration(lex);
        } else {
            statement = parseExpression(lex);
        }
        return statement;
    } else if (cur_type == TK_OPEN_BRACKET) {
        return parseStatementBlock(lex);
    } else if (cur_type == TK_RETURN) {
        return parseReturnStatement(lex);
    } else {
        return parseExpression(lex);
    }

}

StatementBlockAST *parseStatementBlock(Lexer &lex)
{
    if (!lex.checkToken(TK_OPEN_BRACKET)) {
        Error(lex, "Was expecting an open bracket\n");
    }
    lex.consumeToken(); // consume the {
    StatementBlockAST *block = new StatementBlockAST();
    while (!lex.checkToken(TK_CLOSE_BRACKET)) {
        StatementAST *statement = nullptr;
        statement = parseStatement(lex);
        block->statements.push_back(statement);
        if (lex.checkToken(TK_LAST_TOKEN)) {
            Error(lex, "Failed to find a matching close bracket");
        }
    }
    lex.consumeToken(); // match }

    return block;
}


FunctionDefinitionAST *parseFunctionDefinition(Lexer &lex)
{
    FunctionDefinitionAST *fundef = new FunctionDefinitionAST();

    fundef->declaration = parseFunctionDeclaration(lex);
    fundef->function_body = parseStatementBlock(lex);
    return fundef;
}

ExprAST * parseCastExpression(Lexer &lex)
{
    Token t;
    lex.getNextToken(t);

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

ExprAST *parseBinOpExpressionRecursive(Lexer &lex, u32 oldprec, ExprAST *lhs)
{
    TOKEN_TYPE cur_type;

    while (1) {
        cur_type = lex.getTokenType();
        if (isBinOperator(cur_type)) {
            u32 cur_prec = getPrecedence(cur_type);
            if (cur_prec < oldprec) {
                return lhs;
            } else {
                lex.consumeToken();
                ExprAST *rhs = parseCastExpression(lex);
                if (isBinOperator(lex.getTokenType())) {
                    u32 newprec = getPrecedence(lex.getTokenType());
                    if (cur_prec < newprec) {
                        rhs = parseBinOpExpressionRecursive(lex, cur_prec + 1, rhs);
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

ExprAST *parseBinOpExpression(Lexer &lex)
{
    ExprAST *lhs = parseCastExpression(lex);
    return parseBinOpExpressionRecursive(lex, 0, lhs);
}

ExprAST * parseAssignmentExpression(Lexer &lex)
{
    ExprAST *lhs = parseBinOpExpression(lex);
    TOKEN_TYPE type;

    type = lex.getTokenType();
    if (isAssignmentOperator(type)) {
        // @TODO : we would need to have here a RHS evaluation
        AssignAST *assign = new AssignAST();
        assign->lhs = lhs;
        assign->op = type;
        assign->rhs = parseAssignmentExpression(lex);
        return assign;
    } 
    return lhs;
}

ExprAST * parseExpression(Lexer &lex)
{
    Token t;
    lex.lookaheadToken(t);
    if (t.type == TK_OPEN_PAREN) {
        lex.getNextToken(t);
        ExprAST *expr = parseExpression(lex);
        lex.getNextToken(t);
        if (t.type != TK_CLOSE_PAREN) {
            Error(lex, "Cound not find a matching close parentesis\n");
        }
        return expr;
    }
    return parseAssignmentExpression(lex);
}

DefinitionAST *parseDefinition(Lexer &lex)
{
    Token t;
    lex.lookaheadToken(t);
    if (t.type == TK_OPEN_PAREN) {
        // ok, this could be an expression or a function definition!
        // hard to tell, so we will try both and see which one produces a result
        u32 lex_pos = lex.getTokenStreamPosition();
        FunctionDefinitionAST *func_def = parseFunctionDefinition(lex);
        if (func_def != nullptr) {
            return func_def;
        }

        // @TODO: use lookahead instead of token rewind
        // if we are here, means that we could not parse the function definition
        // it has to be an expression, but we need to reset the parsing stream
        lex.setTokenStreamPosition(lex_pos);
        // fall through to the normal expression parsing
    }
    return parseExpression(lex);
}

DeclAST * parseDeclaration(Lexer &lex)
{
    Token t;
    lex.getNextToken(t);
    DeclAST *decl = new DeclAST();

    if (t.type != TK_IDENTIFIER) {
        Error(lex, "Identifier expected but not found\n");
    }

    decl->varname = t.string;
    lex.lookaheadToken(t);
    if (t.type == TK_COLON) {
        lex.consumeToken();
        // we are doing a variable declaration
        decl->specified_type = parseType(lex);
        lex.lookaheadToken(t);
    } 
    
    if (t.type == TK_DOUBLE_COLON) {
        decl->is_constant = true;
    }

    if ((t.type == TK_ASSIGN) || (t.type == TK_DOUBLE_COLON)
        || (t.type == TK_IMPLICIT_ASSIGN)) {
        lex.consumeToken();
        // we are doing an assignment or initial value
        decl->definition = parseDefinition(lex);
        lex.lookaheadToken(t);
    } else if (t.type != TK_SEMICOLON) {
        Error(lex, "Declaration is malformed");
    }
    if (!decl->definition || decl->definition->needsSemiColon()) {
        // @TODO: support compiler flags and others here
        if (t.type != TK_SEMICOLON) {
            Error(lex, "Declaration needs to end with a semicolon\n");
        }
        lex.getNextToken(t);
    }
    return decl;
}

// first version, just return a list of AST
std::vector<BaseAST *> Parse(const char *filename, PoolAllocator *pool)
{
	Lexer lex;
	std::vector<BaseAST *> vec;
    
    lex.setPoolAllocator(pool);

	lex.openFile(filename);
    lex.parseFile();

	while (!lex.checkToken(TK_LAST_TOKEN)) {
		// we got a token, figure out what AST to build
        DeclAST *d = parseDeclaration(lex);
        vec.push_back(d);
                
        //lex.getNextToken(t);
        //t.print();
	}

	return vec;
}