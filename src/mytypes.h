#pragma once

typedef signed char        s8;
typedef signed short       s16;
typedef int                s32;
typedef long long          s64;

typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
#ifdef WIN32
typedef unsigned long long u64;
#else
typedef unsigned long      u64;
#endif
typedef float              f32;
typedef double             f64;