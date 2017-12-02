#include "os.h"

// Most of this code is a heavily modified version of some of the subsystem in Remotery

#ifdef PLATFORM_LINUX
#include <time.h>
#ifdef __FreeBSD__
#include <pthread_np.h>
#else
#include <sys/prctl.h>
#endif
#endif

#if defined(PLATFORM_POSIX)
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/mman.h>
#include <netinet/in.h>
#include <fcntl.h>
#include <errno.h>
#include <dlfcn.h>
#endif

#if defined(PLATFORM_WINDOWS)
#include <windows.h>
#include <intrin.h>
#endif 

#include <assert.h>
#include <stdio.h>
#include <string.h>

void* osLoadLibrary(const char* path)
{
#if defined(PLATFORM_WINDOWS)
    return (void*)LoadLibraryA(path);
#elif defined(PLATFORM_POSIX)
    return dlopen(path, RTLD_LOCAL | RTLD_LAZY);
#else
    return nullptr;
#endif
}

void osFreeLibrary(void* handle)
{
#if defined(PLATFORM_WINDOWS)
    FreeLibrary((HMODULE)handle);
#elif defined(PLATFORM_POSIX)
    dlclose(handle);
#endif
}

void* osGetProcAddress(void* handle, const char* symbol)
{
#if defined(PLATFORM_WINDOWS)
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable:4152) // C4152: nonstandard extension, function/data pointer conversion in expression
#endif
    return GetProcAddress((HMODULE)handle, (LPCSTR)symbol);
#ifdef _MSC_VER
#pragma warning(pop)
#endif
#elif defined(PLATFORM_POSIX)
    return dlsym(handle, symbol);
#else
    return nullptr;
#endif
}

/*
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
@TIMERS: Platform-specific timers
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
*/



//
// Get millisecond timer value that has only one guarantee: multiple calls are consistently comparable.
// On some platforms, even though this returns milliseconds, the timer may be far less accurate.
//
u32 msTimer_Get()
{
#ifdef PLATFORM_WINDOWS
    return (u32)GetTickCount();
#else

    clock_t time = clock();

    // CLOCKS_PER_SEC is 128 on FreeBSD, causing div/0
#ifdef __FreeBSD__
    u32 msTime = (u32)(time * 1000 / CLOCKS_PER_SEC);
#else
    u32 msTime = (u32)(time / (CLOCKS_PER_SEC / 1000));
#endif

    return msTime;

#endif
}



/*
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
@TLS: Thread-Local Storage
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
*/



#define TLS_INVALID_HANDLE 0xFFFFFFFF

osError tlsAlloc(osTLS* handle)
{
    assert(handle != NULL);

#if defined(PLATFORM_WINDOWS)

    *handle = (osTLS)TlsAlloc();
    if (*handle == TLS_OUT_OF_INDEXES)
    {
        *handle = TLS_INVALID_HANDLE;
        return OS_ERROR_TLS_ALLOC_FAIL;
    }

#elif defined(PLATFORM_POSIX)

    if (pthread_key_create((pthread_key_t *)handle, NULL) != 0)
    {
        *handle = TLS_INVALID_HANDLE;
        return OS_ERROR_TLS_ALLOC_FAIL;
    }

#endif

    return OS_ERROR_NONE;
}


void tlsFree(osTLS handle)
{
    assert(handle != TLS_INVALID_HANDLE);

#if defined(PLATFORM_WINDOWS)

    TlsFree(handle);

#elif defined(PLATFORM_POSIX)

    pthread_key_delete((pthread_key_t)handle);

#endif
}


void tlsSet(osTLS handle, void* value)
{
    assert(handle != TLS_INVALID_HANDLE);

#if defined(PLATFORM_WINDOWS)

    TlsSetValue(handle, value);

#elif defined(PLATFORM_POSIX)

    pthread_setspecific((pthread_key_t)handle, value);

#endif
}


void* tlsGet(osTLS handle)
{
    assert(handle != TLS_INVALID_HANDLE);

#if defined(PLATFORM_WINDOWS)

    return TlsGetValue(handle);

#elif defined(PLATFORM_POSIX)

    return pthread_getspecific((pthread_key_t)handle);

#endif
}



/*
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
@ATOMIC: Atomic Operations
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
*/


bool AtomicCompareAndSwap(u32 volatile* val, long old_val, long new_val)
{
#if defined(PLATFORM_WINDOWS) && !defined(__MINGW32__)
    return _InterlockedCompareExchange((long volatile*)val, new_val, old_val) == old_val ? true : false;
#elif defined(PLATFORM_POSIX) || defined(__MINGW32__)
    return __sync_bool_compare_and_swap(val, old_val, new_val) ? true : false;
#endif
}


bool AtomicCompareAndSwapPointer(long* volatile* ptr, long* old_ptr, long* new_ptr)
{
#if defined(PLATFORM_WINDOWS) && !defined(__MINGW32__)
#ifdef _WIN64
    return _InterlockedCompareExchange64((__int64 volatile*)ptr, (__int64)new_ptr, (__int64)old_ptr) == (__int64)old_ptr ? true : false;
#else
    return _InterlockedCompareExchange((long volatile*)ptr, (long)new_ptr, (long)old_ptr) == (long)old_ptr ? true : false;
#endif
#elif defined(PLATFORM_POSIX) || defined(__MINGW32__)
    return __sync_bool_compare_and_swap(ptr, old_ptr, new_ptr) ? false : true;
#endif
}


//
// NOTE: Does not guarantee a memory barrier
// TODO: Make sure all platforms don't insert a memory barrier as this is only for stats
//       Alternatively, add strong/weak memory order equivalents
//
s32 AtomicAdd(s32 volatile* value, s32 add)
{
#if defined(PLATFORM_WINDOWS) && !defined(__MINGW32__)
    return _InterlockedExchangeAdd((long volatile*)value, (long)add);
#elif defined(PLATFORM_POSIX) || defined(__MINGW32__)
    return __sync_fetch_and_add(value, add);
#endif
}


void AtomicSub(s32 volatile* value, s32 sub)
{
    // Not all platforms have an implementation so just negate and add
    AtomicAdd(value, -sub);
}


// Compiler write fences (windows implementation)
void WriteFence()
{
#if defined(PLATFORM_WINDOWS) && !defined(__MINGW32__)
    _WriteBarrier();
#elif defined (__clang__)
    __asm__ volatile("" : : : "memory");
#else
    asm volatile ("" : : : "memory");
#endif
}

int compile_c_into_binary(const char *filename)
{
#if defined(PLATFORM_WINDOWS)
    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    char cmd_line[512] = {};

    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));

    sprintf_s(cmd_line, "cl.exe /nologo %s", filename);

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
#elif defined(PLATFORM_POSIX)
    char cmd_line[512] = {};
    char outfile[64];
    strncpy(outfile, filename, sizeof(outfile));

    char *ext = strrchr(outfile, '.');
    *ext = 0;

    sprintf(cmd_line, "clang %s -g -o %s -lstdc++", filename, outfile);

    int exit_code = system(cmd_line);

    return exit_code;
#endif
}

