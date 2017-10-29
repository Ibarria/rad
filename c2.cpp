// c2.cpp : Defines the entry point for the console application.
//

#define MAJOR_V 0
#define MINOR_V 1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Parser.h"
#include "Timer.h"
#include "c_generator.h"
#include "os.h"

static const char *root_file = nullptr;
bool option_printTokens = false;
bool option_printAST = false;

void usage()
{
	// show usage and version
	printf(" c2 - experimental compiler, v%d.%d\n", MAJOR_V, MINOR_V);
	printf("\nUsage:\n");
	printf("c2 [options] <source file>\n");
    printf("\tOptions:\n");
    printf("\t-tokens: Print lexeical tokens\n");
    printf("\t-ast: Print ast tree\n");
}

void parseOptions(int argc, char **argv)
{
    int i;
    for (i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "-tokens")) {
            option_printTokens = true;
        } else if (!strcmp(argv[i], "-ast")) {
            option_printAST = true;
        } else {
            root_file = argv[i];
        }
    }
    if (!root_file) {
		usage();
		exit(0);
	}
}

void printTime(const char *stage, double time_sec)
{
    double rectified_time = time_sec;
    const char *units = "seconds";
    if (true || time_sec < 0.01) {
        rectified_time = time_sec * 1000.0;
        units = "ms";
    }
    printf("%s took %7.2lf %s\n", stage, rectified_time, units);
}

int main(int argc, char **argv)
{
	parseOptions(argc, argv);
    double astBuildTime, codegenTime, binaryGenTime = 0.0;
    Timer timer;
    timer.startTimer();

    PoolAllocator pool;    
    Parser p;

	FileAST *parsedFile = p.Parse(argv[1], &pool);

    if (!parsedFile) {
        printf("Error during Lexical and Syntactic parsing:\n%s", p.errorString);
        exit(1);
    }

    traverseAST(parsedFile);
    
    astBuildTime = timer.stopTimer();
    timer.startTimer();

    c_generator gen;
    gen.generate_c_file("first.cpp", parsedFile);

    if (option_printAST) {
        printAST(parsedFile, 0);
    }

    codegenTime = timer.stopTimer();
    
    timer.startTimer();
    
    compile_c_into_binary("first.cpp");

    binaryGenTime = timer.stopTimer();

    printTime("     AST building stage", astBuildTime);
    printTime("C Code generation stage", codegenTime);
    printTime("Binary generation stage", binaryGenTime);

    delete parsedFile;
    return 0;
}

