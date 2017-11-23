
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <varargs.h>

struct string {
    char *data;
    unsigned long long size;
};

__declspec(dllexport) int myprintf(string fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    int ret = vprintf_s(fmt.data, args);
    va_end(args);

    return ret;
}