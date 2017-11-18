#pragma once

typedef signed char        s8;
typedef signed short       s16;
typedef int                s32;
typedef long long          s64;

typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
#ifdef WIN32
#define U64FMT             "ll"
typedef unsigned long long u64;
#else
#define U64FMT              "l"
typedef unsigned long      u64;
#endif
typedef float              f32;
typedef double             f64;