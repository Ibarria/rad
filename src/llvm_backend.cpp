// llvm_backend.cpp : Defines the entry point for the console application.
//

#if defined(_WIN32)
#include <windows.h>
#define DLLEXPORT __declspec(dllexport)
#else 
#define DLLEXPORT 
#endif

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "Hash.h"

#ifndef WIN32
# define sprintf_s  sprintf
# define vsprintf_s vsnprintf
# define strncpy_s  strncpy
# define stricmp    strcasecmp
#endif



int link_object(const char *obj_file, ImportsHash &imports)
{
#if defined(PLATFORM_WINDOWS)
    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    char cmd_line[512] = {};

    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));

    //GetCurrentDirectoryA(sizeof(cmd_line), cmd_line);
    //printf("Debug current directory: %s\n", cmd_line);
    //cmd_line[0] = 0;

    // We need msvcrt here, otherwise we have to specify the /ENTRY:<entrypoint> and
    // the program takes a very long time to exit. We would have to build our own global
    // constructors, destructors and call ExitProcess at the end. Basically implementing a basic CRT
    u32 chars_written = sprintf_s(cmd_line, "link.exe /nologo /DEBUG /subsystem:CONSOLE /NODEFAULTLIB .\\%s kernel32.lib user32.lib libcmt.lib libvcruntime.lib libucrt.lib", obj_file);

    auto it = imports.begin();
    char *line_ptr = cmd_line + chars_written;
    while (!imports.isEnd(it)) {
        if (it.entry) {
            chars_written = sprintf(line_ptr, " modules\\%s.lib", it.entry->key());
            line_ptr = line_ptr + chars_written;
        }
        it = imports.next(it);
    }

    //printf("Debug command line: %s\n", cmd_line);

    if (!CreateProcessA(NULL, cmd_line, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
        printf("Process creation failed (%d)\n", GetLastError());
        return -1;
    }

    // Wait until child process exits.
    WaitForSingleObject(pi.hProcess, INFINITE);

    DWORD exit_code;
    GetExitCodeProcess(pi.hProcess, &exit_code);
    // Close process and thread handles. 
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    return exit_code;
#elif defined(PLATFORM_LINUX)
    printf("Unimplemented on Linux\n");
    return -1;
#elif defined(PLATFORM_MACOS)
    char cmd_line[512] = {};
    char outfile[64];
    strncpy(outfile, obj_file, sizeof(outfile));

    char *ext = strrchr(outfile, '.');
    *ext = 0;

    u32 chars_written = sprintf(cmd_line, "clang %s -g -o %s -lstdc++", obj_file, outfile);

    auto it = imports.begin();
    char *line_ptr = cmd_line + chars_written;
    while (!imports.isEnd(it)) {
        if (it.entry) {
            chars_written = sprintf(line_ptr, " modules/%s.dylib", it.entry->key());
            line_ptr = line_ptr + chars_written;
        }
        it = imports.next(it);
    }

    int exit_code = system(cmd_line);

    return exit_code;
#endif
}
