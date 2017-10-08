// c2.cpp : Defines the entry point for the console application.
//

#include <stdio.h>
#include <stdlib.h>
#undef EOF
#include "Lexer.h"

int main(int argc, char **argv)
{
	FileData file;
	Lexer lex;
	Token t;

	lex.openFile("c2.cpp");

	lex.getNextToken(t);

	while (t.type != EOF) {
		t.print();
		lex.getNextToken(t);
	}
    return 0;
}

