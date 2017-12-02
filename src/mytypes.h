#pragma once

#if defined(_WINDOWS) || defined(_WIN32)
#define PLATFORM_WINDOWS
#elif defined(__linux__) || defined(__FreeBSD__)
#define PLATFORM_LINUX
#define PLATFORM_POSIX
#elif defined(__APPLE__)
#define PLATFORM_MACOS
#define PLATFORM_POSIX
#endif

typedef signed char        s8;
typedef signed short       s16;
typedef int                s32;
typedef long long          s64;

typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
#if defined(PLATFORM_WINDOWS)
#define U64FMT             "ll"
typedef unsigned long long u64;
#else
#define U64FMT              "l"
typedef unsigned long      u64;
#endif
typedef float              f32;
typedef double             f64;