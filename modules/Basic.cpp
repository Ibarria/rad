
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#if defined(_WIN32)
#include <varargs.h>
#define DLLEXPORT __declspec(dllexport)
#else 
#include <stdarg.h>
#define DLLEXPORT 
#define vprintf_s vprintf
#endif

struct string {
    char *data;
    unsigned long size;
};

extern "C" DLLEXPORT int print(char *data, unsigned long size, ...)
{
    va_list args;
    va_start(args, size);

    int ret = vprintf_s(data, args);
    va_end(args);

    return ret;
}
