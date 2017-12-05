#include "mytypes.h"
#include "Timer.h"

#if defined(PLATFORM_WINDOWS)
# include <windows.h>
#else 
# include <sys/time.h>
# include <unistd.h>
# if defined(PLATFORM_MACOS)
#include <mach/mach.h>
#include <mach/mach_time.h>
# endif
#endif
#include <stdio.h>

Timer::Timer()
{
#if defined(PLATFORM_WINDOWS)
    LARGE_INTEGER performance_frequency;

    // Calculate the scale from performance counter to microseconds
    QueryPerformanceFrequency(&performance_frequency);
    scale = 1000000.0 / performance_frequency.QuadPart;

#elif defined(PLATFORM_MACOS)

    mach_timebase_info_data_t nsScale;
    mach_timebase_info(&nsScale);
    const double ns_per_us = 1.0e3;
    scale = (double)(nsScale.numer) / ((double)nsScale.denom * ns_per_us);

#elif defined(PLATFORM_LINUX)

#endif
    start_time = 0;

}

Timer::~Timer()
{
}

void Timer::startTimer()
{
#if defined(PLATFORM_WINDOWS)
    LARGE_INTEGER perfCount;
    QueryPerformanceCounter(&perfCount);
    start_time = perfCount.QuadPart;
#elif defined(PLATFORM_MACOS)

    start_time = mach_absolute_time();

#elif defined(PLATFORM_LINUX)

    struct timeval tv;
    gettimeofday(&tv, NULL);
    start_time = tv.tv_sec * 1000000ULL + tv.tv_usec;

#endif
}

/*
This function returns the number of micro seconds it passed since
the timer started, as a u64 value
*/
u64 Timer::stopTimerUs()
{
    u64 ellapsed_time = 0;
#if defined(PLATFORM_WINDOWS)
    LARGE_INTEGER perfCount;
    LARGE_INTEGER li;
    QueryPerformanceCounter(&perfCount);

    u64 end_time = perfCount.QuadPart;
    start_time = end_time - start_time;
    ellapsed_time = start_time;
#elif defined(PLATFORM_MACOS)

    u64 curr_time = mach_absolute_time();
    ellapsed_time = curr_time - start_time;

#elif defined(PLATFORM_LINUX)
    struct timeval tv;
    gettimeofday(&tv, NULL);
    u64 end_time = tv.tv_sec * 1000000ULL + tv.tv_usec;
    start_time = end_time - start_time;
    ellapsed_time = start_time;
#endif	
    return ellapsed_time;
}

/*
    This function returns the number of seconds it passed since 
    the timer started, as a double value
*/
double Timer::stopTimer()
{
	double ellapsed_time = 0;
#if defined(PLATFORM_WINDOWS)
    LARGE_INTEGER perfCount;
    QueryPerformanceCounter(&perfCount);

    u64 end_time = perfCount.QuadPart;
    start_time = end_time - start_time;
    ellapsed_time = (double)start_time / scale;
#elif defined(PLATFORM_MACOS)

    u64 end_time = mach_absolute_time();
    ellapsed_time = ((double)(end_time - start_time) * scale);

#elif defined(PLATFORM_LINUX)
    struct timeval tv;
	gettimeofday(&tv, NULL);
	u64 end_time = tv.tv_sec * 1000000ULL + tv.tv_usec;
	start_time = end_time - start_time;
	ellapsed_time = (double)start_time / 1000000.0; 
#endif	
    
    return ellapsed_time / 10000000.0;
}

