#include "os.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int compile_c_into_binary(const char *filename)
{
    char cmd_line[512] = {};
	char outfile[64];
	strncpy(outfile, filename, sizeof(outfile));

	char *ext = strrchr(outfile, '.');
	*ext = 0;

    sprintf(cmd_line, "clang %s -o %s -lstdc++", filename, outfile);

	int exit_code = system(cmd_line);

    return exit_code;
}