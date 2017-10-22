#include "Parser.h"
#include "Lexer.h"
#include <string>

TypeAST *parseFunctionDeclaration(Lexer &lex);
FunctionDefinitionAST *parseFunctionDefinition(Lexer &lex);

ExprAST * parseAssignmentExpression(Lexer & lex);

static void Error(const Lexer &lex, const char *msg)
{
	SrcLocation loc;
	lex.getLocation(loc);
	printf("Error %s:%lld - %s", lex.getFilename(), loc.line, msg);
	exit(1);
}

static bool isAssignmentOperator(const Token &t)
{
    return (t.type == ASSIGN)
        || (t.type == MUL_ASSIGN)
        || (t.type == DIV_ASSIGN)
        || (t.type == MOD_ASSIGN)
        || (t.type == ADD_ASSIGN)
        || (t.type == SUB_ASSIGN)
        || (t.type == LEFT_ASSIGN)
        || (t.type == RIGHT_ASSIGN)
        || (t.type == AND_ASSIGN)
        || (t.type == XOR_ASSIGN)
        || (t.type == OR_ASSIGN);
}

TypeAST *parseDirectType(Lexer &lex)
{
    // @TODO: support pointer, arrays, etc
    Token t;
    lex.getNextToken(t);
    if (t.type != IDENTIFIER) {
        Error(lex, "Type can only be an identifier, for now\n");
    }
    DirectTypeAST *type = new DirectTypeAST();
    type->name = t.str;
    if (t.str == "string") {
        type->type = DirectTypeAST::STRING;
    } else if (t.str == "int") {
        type->type = DirectTypeAST::I64;
    } else if (t.str == "float") {
        type->type = DirectTypeAST::F32;
    } else if (t.str == "i8") {
        type->type = DirectTypeAST::I8;
    } else if (t.str == "i16") {
        type->type = DirectTypeAST::I16;
    } else if (t.str == "i32") {
        type->type = DirectTypeAST::I32;
    } else if (t.str == "i64") {
        type->type = DirectTypeAST::I64;
    } else if (t.str == "u8") {
        type->type = DirectTypeAST::U8;
    } else if (t.str == "u16") {
        type->type = DirectTypeAST::U16;
    } else if (t.str == "u32") {
        type->type = DirectTypeAST::U32;
    } else if (t.str == "u64") {
        type->type = DirectTypeAST::U64;
    } 
    // @TODO: support custom types

    return type;
}

TypeAST * parseType(Lexer &lex)
{
    Token t;
    lex.lookaheadToken(t);
    if (t.type == OPEN_PAREN) {
        return parseFunctionDeclaration(lex);
    }

    return parseDirectType(lex);
}

TypeAST *parseFunctionDeclaration(Lexer &lex) 
{
    // @TODO: implement me
    return nullptr;
}

FunctionDefinitionAST *parseFunctionDefinition(Lexer &lex)
{
    return nullptr;
}

ExprAST * parseCastExpression(Lexer &lex)
{
    Token t;
    lex.getNextToken(t);

    if (t.type == IDENTIFIER) {

    } else if (t.type == NUMBER) {

    }
    return nullptr;
}

ExprAST * parseAssignmentExpression(Lexer &lex)
{
    ExprAST *lhs = parseCastExpression(lex);
    Token t;

    lex.lookaheadToken(t);
    if (isAssignmentOperator(t)) {

    }
    return nullptr;
}

ExprAST * parseExpression(Lexer &lex)
{
    Token t;
    lex.lookaheadToken(t);
    if (t.type == OPEN_PAREN) {
        lex.getNextToken(t);
        ExprAST *expr = parseExpression(lex);
        lex.getNextToken(t);
        if (t.type != CLOSE_PAREN) {
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
    if (t.type == OPEN_PAREN) {
        // ok, this could be an expression or a function definition!
        // hard to tell, so we will try both and see which one produces a result
        u32 lex_pos = lex.getTokenStreamPosition();
        FunctionDefinitionAST *func_def = parseFunctionDefinition(lex);
        if (func_def != nullptr) {
            return func_def;
        }
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
    DeclAST *decl = new DeclAST;

    if (t.type != IDENTIFIER) {
        Error(lex, "Identifier expected but not found\n");
    }

    decl->varname = t.str;
    lex.getNextToken(t);
    if (t.type == COLON) {
        // we are doing a variable declaration
        decl->specified_type = parseType(lex);
        lex.getNextToken(t);
    } 
    
    if (t.type == DOUBLE_COLON) {
        decl->is_constant = true;
    }

    if ((t.type == ASSIGN) || (t.type == DOUBLE_COLON)) {
        // we are doing an assignment or initial value
        decl->definition = parseDefinition(lex);
    }
    lex.getNextToken(t);
    // @TODO: support compiler flags and others here
    if (t.type != SEMICOLON) {
        Error(lex, "Declaration needs to end with a semicolon\n");
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

	while (t.type != LAST_TOKEN) {
		// we got a token, figure out what AST to build
        //DeclAST *d = parseDeclaration(lex);
        //vec.push_back(d);
        lex.getNextToken(t);
        t.print();
	}

	return vec;
}