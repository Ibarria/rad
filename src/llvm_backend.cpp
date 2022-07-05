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
#include "FileObject.h"

#ifndef WIN32
# define sprintf_s  sprintf
# define vsprintf_s vsnprintf
# define strncpy_s  strncpy
# define stricmp    strcasecmp
#else
struct Find_Result {
    int windows_sdk_version;   // Zero if no Windows SDK found.

    wchar_t* windows_sdk_root = NULL;
    wchar_t* windows_sdk_um_library_path = NULL;
    wchar_t* windows_sdk_ucrt_library_path = NULL;

    wchar_t* vs_exe_path = NULL;
    wchar_t* vs_library_path = NULL;
};

Find_Result find_visual_studio_and_windows_sdk();

void free_resources(Find_Result* result);
#endif



int link_object(FileObject &obj_file, ImportsHash &imports, const char* output_name, bool option_debug_info)
{
#if defined(PLATFORM_WINDOWS)
    STARTUPINFO si;
    PROCESS_INFORMATION pi;
    static const int CMD_SIZE = 4096;
    wchar_t cmd_line[CMD_SIZE] = {};
    wchar_t out_cmd[512] = {};

    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));

    Find_Result vspath = find_visual_studio_and_windows_sdk();
    //GetCurrentDirectoryA(sizeof(cmd_line), cmd_line);
    //printf("Debug current directory: %s\n", cmd_line);
    //cmd_line[0] = 0;

    if (output_name) {
        swprintf_s(out_cmd, sizeof(out_cmd) / sizeof(out_cmd[0]), L"/OUT:%hs", output_name);
    }

    // We need msvcrt here, otherwise we have to specify the /ENTRY:<entrypoint> and
    // the program takes a very long time to exit. We would have to build our own global
    // constructors, destructors and call ExitProcess at the end. Basically implementing a basic CRT
    u32 chars_written = swprintf_s(cmd_line, CMD_SIZE,
        L"\"%s\\link.exe\" /nologo %hs /INCREMENTAL:NO /subsystem:CONSOLE /NODEFAULTLIB /ENTRY:_start "
        L"/LIBPATH:\"%s\" /LIBPATH:\"%s\" /LIBPATH:\"%s\" /LIBPATH:\"%s\" %s %hs kernel32.lib user32.lib libcmt.lib libvcruntime.lib libucrt.lib"
        L" modules\\start_win.obj", 
        vspath.vs_exe_path, option_debug_info ? "/DEBUG" : "", vspath.vs_library_path, vspath.windows_sdk_root, vspath.windows_sdk_ucrt_library_path, vspath.windows_sdk_um_library_path,
        out_cmd, obj_file.getFilename());

    auto it = imports.begin();
    wchar_t *line_ptr = cmd_line + chars_written;
    while (!imports.isEnd(it)) {
        if (it.entry) {
            chars_written = swprintf(line_ptr, CMD_SIZE - chars_written, L" modules\\%hs.lib", it.entry->key());
            line_ptr = line_ptr + chars_written;
        }
        it = imports.next(it);
    }

    // TODO: Always include basic. It would be better if we checked if basic was there already or not
    chars_written = swprintf(line_ptr, CMD_SIZE - chars_written, L" modules\\Basic.lib");
    line_ptr = line_ptr + chars_written;

    // printf("Debug command line: %S\n", cmd_line);

    if (!CreateProcessW(NULL, cmd_line, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
        printf("Process creation [%ls] failed (%d)\n", cmd_line, GetLastError());
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
    char cmd_line[512] = {};
    FileObject outfile; 
    if (output_name) outfile.setFile(output_name);
    else {
        outfile.setFile(obj_file.getName());
        outfile.setExtension("");
    }

    u32 chars_written = sprintf(cmd_line, 
        "clang %s %s -o %s -ldl -lstdc++ -nodefaultlibs",
        obj_file.getFilename(), option_debug_info ? "-g" : "", outfile.getFilename());

    auto it = imports.begin();
    char *line_ptr = cmd_line + chars_written;
    while (!imports.isEnd(it)) {
        if (it.entry) {
            chars_written = sprintf(line_ptr, " modules/%s.so", it.entry->key());
            line_ptr = line_ptr + chars_written;
        }
        it = imports.next(it);
    }

    int exit_code = system(cmd_line);

    return exit_code;
#elif defined(PLATFORM_MACOS)
    char cmd_line[512] = {};
    char outfile[64];
    strncpy(outfile, obj_file.getFilename(), sizeof(outfile));

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
