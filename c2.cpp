// c2.cpp : Defines the entry point for the console application.
//

#define MAJOR_V 0
#define MINOR_V 1

#include <stdio.h>
#include <stdlib.h>
#include "Parser.h"
#include "Timer.h"

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
	parseOptions(argc, argv);
    Timer timer;
    timer.startTimer();

    PoolAllocator pool;    
    Parser p;

	Array<BaseAST *> *vec = p.Parse(argv[1], &pool);

	for (auto ast : *vec) {
		ast->print(0);
	}

    timer.stopTimer();
    timer.printTimeEllapsed();

    delete vec;
    return 0;
}

