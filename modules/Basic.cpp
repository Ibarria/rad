
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#if defined(_WIN32)
#include <varargs.h>
#include <windows.h>
#define DLLEXPORT __declspec(dllexport)
#else 
#include <stdarg.h>
#include <unistd.h>
#define DLLEXPORT 
#define vprintf_s vprintf
#endif

// Using code from Sean Barret
#define STB_SPRINTF_IMPLEMENTATION
#include "stb_sprintf.h"

struct string {
    char *data;
    unsigned long size;
};

extern "C" DLLEXPORT void func()
{
    return;
}

extern "C" DLLEXPORT void * _malloc(unsigned long long size)
{
#if defined(_WIN32)
    return HeapAlloc(GetProcessHeap(), 0, size);
#else
#error "Implement this"
#endif
}

extern "C" DLLEXPORT void _free(void *ptr)
{
#if defined(_WIN32)
    HeapFree(GetProcessHeap(), 0, ptr);
#else
#error "Implement this"
#endif
}

int _strlen(const char *s)
{
    int r = 0;
    while (*s++) r++;
    return r;
}

extern "C" DLLEXPORT int print(char *data, unsigned long long size, ...)
{
#if defined(_WIN32)
    DWORD dwRet;
    char buffer[256];

    va_list args;
    va_start(args, size);

    stbsp_vsnprintf(buffer, sizeof(buffer), data, args);
    WriteConsole(GetStdHandle(STD_OUTPUT_HANDLE), buffer, _strlen(buffer), &dwRet, 0);

    // int ret = vprintf_s(data, args);
    va_end(args);

    return dwRet;
#else
	int ret = 0;
    char buffer[256];

    va_list args;
    va_start(args, size);

    vsprintf(buffer, data, args);
	ret = write(1, buffer, strlen(buffer));

    // int ret = vprintf_s(data, args);
    va_end(args);
	return ret;
#endif
}

#if defined(_WIN32)

extern "C" BOOL WINAPI DllMain(
    _In_ HINSTANCE hinstDLL,
    _In_ DWORD     fdwReason,
    _In_ LPVOID    lpvReserved
) 
{
    switch( fdwReason ) 
    { 
        case DLL_PROCESS_ATTACH:
         // Initialize once for each new process.
         // Return FALSE to fail DLL load.
         // print("Process Attach\n", 0);
            break;

        case DLL_THREAD_ATTACH:
         // Do thread-specific initialization.
         // print("Thread Attach\n", 0);
            break;

        case DLL_THREAD_DETACH:
         // Do thread-specific cleanup.
         // print("Thread Dettach\n", 0);
            break;

        case DLL_PROCESS_DETACH:
         // Perform any necessary cleanup.
         // print("Process Dettach\n", 0);
            break;
    }
    return TRUE;
}

#include "ProcessThreadsAPI.h"
#endif 
extern "C" DLLEXPORT void end()
{
#if defined(_WIN32)
    // print("Calling exit process now\n", 10);
    ExitProcess(0);
#else
	_exit(0);
#endif	
}

extern "C" DLLEXPORT void _abort()
{
#if defined(_WIN32)
    __debugbreak();
    end();
#else
    abort();
#endif  
}
