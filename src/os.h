#pragma once
#include "mytypes.h"
#include "Hash.h"
#include "FileObject.h"

// All possible error codes
enum osError
{
    OS_ERROR_NONE,

    // System errors
    OS_ERROR_MALLOC_FAIL,                      
    OS_ERROR_TLS_ALLOC_FAIL,                   
    OS_ERROR_CREATE_THREAD_FAIL,               

};

void* osLoadLibrary(const char* path);
void osFreeLibrary(void* handle);
void* osGetProcAddress(void* handle, const char* symbol);

u32 msTimer_Get();

typedef u32 osTLS;
osError tlsAlloc(osTLS* handle);
void tlsFree(osTLS handle);
void tlsSet(osTLS handle, void* value);
void* tlsGet(osTLS handle);

bool AtomicCompareAndSwap(u32 volatile* val, long old_val, long new_val);
bool AtomicCompareAndSwapPointer(long* volatile* ptr, long* old_ptr, long* new_ptr);
s32 AtomicAdd(s32 volatile* value, s32 add);
void AtomicSub(s32 volatile* value, s32 sub);
void WriteFence();

u64 osGetCurrentProcessId();
u64 osGetCurrentThreadId();

int compile_c_into_binary(FileObject &filename, ImportsHash &imports);
