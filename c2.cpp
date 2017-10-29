// c2.cpp : Defines the entry point for the console application.
//

#define MAJOR_V 0
#define MINOR_V 1

#include <stdio.h>
#include <stdlib.h>
#include "Parser.h"
#include "Timer.h"
#include "c_generator.h"
#include "os.h"

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

void printTime(const char *stage, double time_sec)
{
    double rectified_time = time_sec;
    const char *units = "seconds";
    if (time_sec < 0.01) {
        rectified_time = time_sec * 1000.0;
        units = "ms";
    }
    printf("%s took %lf %s\n", stage, rectified_time, units);
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

    traverseAST(parsedFile);
    
    astBuildTime = timer.stopTimer();
    timer.startTimer();

    c_generator gen;
    gen.generate_c_file("first.cpp", parsedFile);
    // printAST(parsedFile, 0);

    codegenTime = timer.stopTimer();
    
    timer.startTimer();
    
    compile_c_into_binary("first.cpp");

    binaryGenTime = timer.stopTimer();

    printTime("AST building stage", astBuildTime);
    printTime("C Code generation stage", codegenTime);
    printTime("Binary generation stage", binaryGenTime);

    delete parsedFile;
    return 0;
}

