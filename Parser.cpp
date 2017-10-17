#include "Parser.h"
#undef EOF
#include "Lexer.h"
#include <string>


static void Error(const Lexer &lex, const char *msg)
{
	SrcLocation loc;
	lex.getLocation(loc);
	printf("Error %s:%lld - %s", lex.getFilename(), loc.line, msg);
	exit(1);
}

// first version, just return a list of AST
std::vector<BaseAST *> Parse(const char *filename)
{
	Lexer lex;
	Token t;
	std::vector<BaseAST *> vec;

	lex.openFile(filename);

	lex.getNextToken(t);

	while (t.type != EOF) {
		// we got a token, figure out what AST to build
		// we are now on top level, we only accept:
		// Function definition, declaration [FUTURE]
		// Variable declaration
		// Assignment

		if (t.type != IDENTIFIER) {
			Error(lex, "Identifier expected but not found\n");
		}

		std::string name = t.pl.pstr;
		lex.getNextToken(t);
		if (t.type == COLON) {
			// we are doing a variable declaration
		} else if (t.type == ASSIGN) {
			// we are doing an assignment
		}
	}

	return vec;
}