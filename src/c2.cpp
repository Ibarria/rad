// c2.cpp : Defines the entry point for the console application.
//

#define MAJOR_V 0
#define MINOR_V 1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Interpreter.h"
#include "Parser.h"
#include "Timer.h"
#include "c_generator.h"
#include "os.h"

static const char *root_file = nullptr;
bool option_printTokens = false;
bool option_printAST = false;
bool option_printBytecode = false;

#ifndef WIN32
# define sprintf_s  sprintf
# define vsprintf_s vsnprintf
# define strncpy_s  strncpy
#endif

void usage()
{
	// show usage and version
	printf(" c2 - experimental compiler, v%d.%d\n", MAJOR_V, MINOR_V);
	printf("\nUsage:\n");
	printf("c2 [options] <source file>\n");
    printf("\tOptions:\n");
    printf("\t-tokens: Print lexeical tokens\n");
    printf("\t-ast: Print ast tree\n");
    printf("\t-bytecode: Print bytecode\n");
}

void parseOptions(int argc, char **argv)
{
    int i;
    for (i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "-tokens")) {
            option_printTokens = true;
        } else if (!strcmp(argv[i], "-ast")) {
            option_printAST = true;
        } else if (!strcmp(argv[i], "-bytecode")) {
            option_printBytecode = true;
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

static char c_filename[256];

char *getCfilename(const char *jai_name)
{
    strncpy_s(c_filename, jai_name, sizeof(c_filename));
    char *s = c_filename + strlen(c_filename);
    while ((*s != '.') && (s != c_filename)) s--;
    if (*s == '.') {
        *(++s) = 'c';
        *(++s) = 'p';
        *(++s) = 'p';
        *(++s) = 0;
        return c_filename;
    } else {
        char *s = c_filename + strlen(c_filename);
        *(++s) = '.';
        *(++s) = 'c';
        *(++s) = 'p';
        *(++s) = 'p';
        *(++s) = 0;
        return c_filename;
    }
}

int main(int argc, char **argv)
{
	parseOptions(argc, argv);
    double astBuildTime, codegenTime, binaryGenTime = 0.0;
    Timer timer;
    timer.startTimer();

    Interpreter interp;
    Parser p;

	FileAST *parsedFile = p.Parse(root_file, &interp.pool);

    if (!parsedFile) {
        printf("Error during Lexical and Syntactic parsing:\n%s", p.errorString);
        exit(1);
    }

    if (option_printAST) {
        printAST(parsedFile, 0);
    }

    interp.semanticProcess(parsedFile);

    if (!interp.success) {
        interp.printErrors();
        printf("There were errors during the semantic analysis. Exiting...\n");
        exit(1);
    }

    astBuildTime = timer.stopTimer();

    timer.startTimer();

    c_generator gen;
    char *c_filename = getCfilename(root_file);
    gen.generate_c_file(c_filename, parsedFile);

    codegenTime = timer.stopTimer();
    
    timer.startTimer();
    
    int res = compile_c_into_binary(c_filename);

    binaryGenTime = timer.stopTimer();

    if (res != 0) {
        printf("The C compilation failed with error code: %d\n", res);
    }

    printTime("     AST building stage", astBuildTime);
    printTime("C Code generation stage", codegenTime);
    printTime("Binary generation stage", binaryGenTime);

    return 0;
}

