#include <windows.h>
#include "os.h"
#include <stdio.h>

int compile_c_into_binary(const char *filename)
{
    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    char cmd_line[512] = {};

    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));

    sprintf_s(cmd_line, "cl.exe /nologo %s", filename);

    if (!CreateProcessA(NULL, cmd_line, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
        printf("Process creation failed\n");
        return - 1;
    }

    // Wait until child process exits.
    WaitForSingleObject(pi.hProcess, INFINITE);

    DWORD exit_code;
    GetExitCodeProcess(pi.hProcess, &exit_code);
    // Close process and thread handles. 
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    return exit_code;
}