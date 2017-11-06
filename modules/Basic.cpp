
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <varargs.h>

__declspec(dllexport) int myprintf(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    int ret = vprintf(fmt, args);
    va_end(args);

    return ret;
}