#pragma once

#include <stdint.h>
#include <inttypes.h>

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
typedef int64_t            s64;

typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
#define U64FMT             PRIu64
typedef uint64_t           u64;

typedef float              f32;
typedef double             f64;