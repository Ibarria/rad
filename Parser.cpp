#include "Parser.h"
#include "Lexer.h"
#include <string>

TypeAST *parseFunctionDefinition(Lexer &lex);

static void Error(const Lexer &lex, const char *msg)
{
	SrcLocation loc;
	lex.getLocation(loc);
	printf("Error %s:%lld - %s", lex.getFilename(), loc.line, msg);
	exit(1);
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
        return parseFunctionDefinition(lex);
    }

    return parseDirectType(lex);
}

TypeAST *parseFunctionDefinition(Lexer &lex) 
{
    // @TODO: implement me
    return nullptr;
}

DefinitionAST *parseDefinition(Lexer &lex)
{
    Token t;
    lex.lookaheadToken(t);
    if (t.type == OPEN_PAREN) {
        
        // return parseFunctionDefinition(lex);
    }
    
    return nullptr;
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