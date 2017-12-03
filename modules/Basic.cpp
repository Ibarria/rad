
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <varargs.h>

struct string {
    char *data;
    unsigned long long size;
};

__declspec(dllexport) int myprintf(char *data, unsigned long long size, ...)
{
    va_list args;
    va_start(args, size);

    int ret = vprintf_s(data, args);
    va_end(args);

    return ret;
}