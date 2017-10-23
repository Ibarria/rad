#include "Parser.h"
#include "Lexer.h"
#include <string>

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

static bool isAssignmentOperator(const Token &t)
{
    return (t.type == TK_ASSIGN)
        || (t.type == TK_MUL_ASSIGN)
        || (t.type == TK_DIV_ASSIGN)
        || (t.type == TK_MOD_ASSIGN)
        || (t.type == TK_ADD_ASSIGN)
        || (t.type == TK_SUB_ASSIGN)
        || (t.type == TK_LEFT_ASSIGN)
        || (t.type == TK_RIGHT_ASSIGN)
        || (t.type == TK_AND_ASSIGN)
        || (t.type == TK_XOR_ASSIGN)
        || (t.type == TK_OR_ASSIGN);
}

static bool isBinOperator(const Token &t)
{
    return (t.type == TK_EQ)
        || (t.type == TK_LEQ)
        || (t.type == TK_GEQ)
        || (t.type == TK_NEQ)
        || (t.type == TK_LT)
        || (t.type == TK_GT)
        || (t.type == TK_RSHIFT)
        || (t.type == TK_LSHIFT)
        || (t.type == TK_STAR)
        || (t.type == TK_DIV)
        || (t.type == TK_MOD)
        || (t.type == TK_PIPE)
        || (t.type == TK_DOUBLE_PIPE)
        || (t.type == TK_HAT)
        || (t.type == TK_AMP)
        || (t.type == TK_DOUBLE_AMP)
        || (t.type == TK_PLUS)
        || (t.type == TK_MINUS);
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
    type->name = t.str;
    if (t.str == "string") {
        type->isString = true;
    } else if (t.str == "int") {
        type->type = I64;
    } else if (t.str == "float") {
        type->type = F32;
    } else if (t.str == "i8") {
        type->type = I8;
    } else if (t.str == "i16") {
        type->type = I16;
    } else if (t.str == "i32") {
        type->type = I32;
    } else if (t.str == "i64") {
        type->type = I64;
    } else if (t.str == "u8") {
        type->type = U8;
    } else if (t.str == "u16") {
        type->type = U16;
    } else if (t.str == "u32") {
        type->type = U32;
    } else if (t.str == "u64") {
        type->type = U64;
    } 
    // @TODO: support custom types

    return type;
}

TypeAST * parseType(Lexer &lex)
{
    Token t;
    lex.lookaheadToken(t);
    if (t.type == TK_OPEN_PAREN) {
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
    arg->name = t.str;

    lex.getNextToken(t);
    if (t.type != TK_COLON) {
        Error(lex, "Argument declaration needs a colon between identifier and type\n");
    }

    arg->type = parseType(lex);
    return arg;
}

FunctionDeclarationAST *parseFunctionDeclaration(Lexer &lex)
{
    Token t;
    lex.getNextToken(t);
    if (t.type != TK_OPEN_PAREN) {
        Error(lex, "Function declarations need a parenthesis\n");
    }
    FunctionDeclarationAST *fundec = new FunctionDeclarationAST();
    lex.lookaheadToken(t);
    while (t.type != TK_CLOSE_PAREN) {
        ArgumentDeclarationAST *arg = parseArgumentDeclaration(lex);
        fundec->arguments.push_back(arg);
        lex.lookaheadToken(t);
        if (t.type == TK_COMMA) {
            lex.consumeToken();
        }
    }
    lex.consumeToken();
    // Now do the return value, which can be empty
    lex.lookaheadToken(t);
    if (t.type == TK_RETURN_ARROW) {
        lex.consumeToken();
        fundec->return_type = parseType(lex);
    }
    return fundec;
}

ReturnStatementAST *parseReturnStatement(Lexer &lex)
{
    ReturnStatementAST *ret = new ReturnStatementAST();
    Token t;
    lex.getNextToken(t);
    if (t.type != TK_RETURN) {
        Error(lex, "A return statement needs the 'return' keyword at the start\n");
    }
    ret->ret = parseExpression(lex);
    lex.getNextToken(t);
    if (t.type != TK_SEMICOLON) {
        Error(lex, "Statement needs to end in semicolon\n");
    }
    return ret;
}

StatementAST *parseStatement(Lexer &lex)
{
    Token t;
    lex.lookaheadToken(t);
    StatementAST *statement = nullptr;

    if (t.type == TK_IDENTIFIER) {
        // could be a variable definition or a statement
        Token tnext;
        lex.lookNaheadToken(tnext, 1);
        if ((tnext.type == TK_COLON)
            || (tnext.type == TK_DOUBLE_COLON)
            || (tnext.type == TK_IMPLICIT_ASSIGN)) {
            statement = parseDeclaration(lex);
        } else {
            statement = parseExpression(lex);
        }
        return statement;
    } else if (t.type == TK_OPEN_BRACKET) {
        return parseStatementBlock(lex);
    } else if (t.type == TK_RETURN) {
        return parseReturnStatement(lex);
    } else {
        return parseExpression(lex);
    }

}

StatementBlockAST *parseStatementBlock(Lexer &lex)
{
    Token t;
    lex.getNextToken(t);
    if (t.type != TK_OPEN_BRACKET) {
        Error(lex, "Was expecting an open bracket\n");
    }
    StatementBlockAST *block = new StatementBlockAST();
    lex.lookaheadToken(t);
    while (t.type != TK_CLOSE_BRACKET) {
        StatementAST *statement = nullptr;
        statement = parseStatement(lex);
        block->statements.push_back(statement);
        lex.lookaheadToken(t);
        if (t.type == TK_LAST_TOKEN) {
            Error(lex, "Failed to find a matching close bracket");
        }
    }
    lex.getNextToken(t);

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
        ex->name = t.str;
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
        str->str = t.str;
        return str;
    }
    //@TODO: have an error message here
    printf("Could not parse a expression!");
    return nullptr;
}

ExprAST *parseMultiplicativeExpression(Lexer &lex)
{
    ExprAST *lhs = parseCastExpression(lex);
    Token t;

    lex.lookaheadToken(t);
    if ((t.type == TK_STAR) || (t.type == TK_DIV)
        || (t.type == TK_MOD)) {
        BinOpAST *bin = new BinOpAST();
        bin->lhs = lhs;
        bin->op = t.type;
        lex.consumeToken();
        bin->rhs = parseMultiplicativeExpression(lex);
        return bin;
    }
    return lhs;
}

ExprAST *parseAdditiveExpression(Lexer &lex)
{
    ExprAST *lhs = parseMultiplicativeExpression(lex);
    Token t;

    lex.lookaheadToken(t);
    if ((t.type == TK_PLUS) || (t.type == TK_MINUS)) {
        BinOpAST *bin = new BinOpAST();
        bin->lhs = lhs;
        bin->op = t.type;
        lex.consumeToken();
        bin->rhs = parseAdditiveExpression(lex);
        return bin;
    }
    return lhs;
}

ExprAST *parseShiftExpression(Lexer &lex)
{
    ExprAST *lhs = parseAdditiveExpression(lex);
    Token t;

    lex.lookaheadToken(t);
    if ((t.type == TK_LSHIFT) || (t.type == TK_RSHIFT)) {
        BinOpAST *bin = new BinOpAST();
        bin->lhs = lhs;
        bin->op = t.type;
        lex.consumeToken();
        bin->rhs = parseShiftExpression(lex);
        return bin;
    }
    return lhs;
}

ExprAST *parseRelationalExpression(Lexer &lex)
{
    ExprAST *lhs = parseShiftExpression(lex);
    Token t;

    lex.lookaheadToken(t);
    if ((t.type == TK_LEQ) || (t.type == TK_GEQ)
        || (t.type == TK_LT) || (t.type == TK_GT)) {
        BinOpAST *bin = new BinOpAST();
        bin->lhs = lhs;
        bin->op = t.type;
        lex.consumeToken();
        bin->rhs = parseRelationalExpression(lex);
        return bin;
    }
    return lhs;
}

ExprAST *parseEqualityExpression(Lexer &lex)
{
    ExprAST *lhs = parseRelationalExpression(lex);
    Token t;

    lex.lookaheadToken(t);
    if ((t.type == TK_EQ) || (t.type == TK_NEQ)) {
        BinOpAST *bin = new BinOpAST();
        bin->lhs = lhs;
        bin->op = t.type;
        lex.consumeToken();
        bin->rhs = parseEqualityExpression(lex);
        return bin;
    }
    return lhs;
}

ExprAST *parseBinaryAndExpression(Lexer &lex)
{
    ExprAST *lhs = parseEqualityExpression(lex);
    Token t;

    lex.lookaheadToken(t);
    if (t.type == TK_AMP) {
        BinOpAST *bin = new BinOpAST();
        bin->lhs = lhs;
        bin->op = t.type;
        lex.consumeToken();
        bin->rhs = parseBinaryAndExpression(lex);
        return bin;
    }
    return lhs;
}

ExprAST *parseBinaryXorExpression(Lexer &lex)
{
    ExprAST *lhs = parseBinaryAndExpression(lex);
    Token t;

    lex.lookaheadToken(t);
    if (t.type == TK_HAT) {
        BinOpAST *bin = new BinOpAST();
        bin->lhs = lhs;
        bin->op = t.type;
        lex.consumeToken();
        bin->rhs = parseBinaryXorExpression(lex);
        return bin;
    }
    return lhs;
}

ExprAST *parseBinaryOrExpression(Lexer &lex)
{
    ExprAST *lhs = parseBinaryXorExpression(lex);
    Token t;

    lex.lookaheadToken(t);
    if (t.type == TK_PIPE) {
        BinOpAST *bin = new BinOpAST();
        bin->lhs = lhs;
        bin->op = t.type;
        lex.consumeToken();
        bin->rhs = parseBinaryOrExpression(lex);
        return bin;
    }
    return lhs;
}

ExprAST *parseLogicalAndExpression(Lexer &lex)
{
    ExprAST *lhs = parseBinaryOrExpression(lex);
    Token t;

    lex.lookaheadToken(t);
    if (t.type == TK_DOUBLE_AMP) {
        BinOpAST *bin = new BinOpAST();
        bin->lhs = lhs;
        bin->op = t.type;
        lex.consumeToken();
        bin->rhs = parseLogicalAndExpression(lex);
        return bin;
    }
    return lhs;
}

ExprAST *parseLogicalOrExpression(Lexer &lex)
{
    ExprAST *lhs = parseLogicalAndExpression(lex);
    Token t;

    lex.lookaheadToken(t);
    if (t.type == TK_DOUBLE_PIPE) {
        BinOpAST *bin = new BinOpAST();
        bin->lhs = lhs;
        bin->op = t.type;
        lex.consumeToken();
        bin->rhs = parseLogicalOrExpression(lex);
        return bin;
    }
    return lhs;
}

ExprAST * parseAssignmentExpression(Lexer &lex)
{
    ExprAST *lhs = parseLogicalOrExpression(lex);
    Token t;

    lex.lookaheadToken(t);
    if (isAssignmentOperator(t)) {
        // @TODO : we would need to have here a RHS evaluation
        AssignAST *assign = new AssignAST();
        assign->lhs = lhs;
        assign->op = t.type;
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

    decl->varname = t.str;
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
std::vector<BaseAST *> Parse(const char *filename)
{
	Lexer lex;
	Token t;
	std::vector<BaseAST *> vec;

	lex.openFile(filename);
    lex.parseFile();
	lex.lookaheadToken(t);

	while (t.type != TK_LAST_TOKEN) {
		// we got a token, figure out what AST to build
        DeclAST *d = parseDeclaration(lex);
        vec.push_back(d);
        
        lex.lookaheadToken(t);
        
        //lex.getNextToken(t);
        //t.print();
	}

	return vec;
}