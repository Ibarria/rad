// c2.cpp : Defines the entry point for the console application.
//

#define MAJOR_V 0
#define MINOR_V 1

#include <stdio.h>
#include <stdlib.h>
#undef EOF
#include "Lexer.h"

void usage()
{
	// show usage and version
	printf(" c2 - experimental compiler, v%d.%d\n", MAJOR_V, MINOR_V);
	printf("\nUsage:\n");
	printf("c2 <source file>\n");
}

void parseOptions(int argc, char **argv)
{
	if (argc == 1) {
		usage();
		exit(0);
	}
}

int main(int argc, char **argv)
{
	FileData file;
	Lexer lex;
	Token t;

	parseOptions(argc, argv);

	lex.openFile(argv[1]);

	lex.getNextToken(t);

	while (t.type != EOF) {
		t.print();
		lex.getNextToken(t);
	}
    return 0;
}

