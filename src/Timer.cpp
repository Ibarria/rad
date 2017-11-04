#include "Timer.h"

#ifdef WIN32
# include <windows.h>
#else 
# include <sys/time.h>
# include <unistd.h>
#endif
#include <stdio.h>

Timer::Timer()
{
    start_time = 0;
}

Timer::~Timer()
{
}

void Timer::startTimer()
{
#ifdef WIN32	
    LARGE_INTEGER perfCount;
    QueryPerformanceCounter(&perfCount);
    start_time = perfCount.QuadPart;
#else
	struct timeval tv;
	gettimeofday(&tv, NULL);
	start_time = tv.tv_sec * 1000000ULL + tv.tv_usec;
#endif	
}

double Timer::stopTimer()
{
	double ellapsed_time = 0;
#ifdef WIN32	
    LARGE_INTEGER perfCount;
    LARGE_INTEGER li;
    QueryPerformanceCounter(&perfCount);
    QueryPerformanceFrequency(&li);

    unsigned long long end_time = perfCount.QuadPart;
    start_time = end_time - start_time;
    ellapsed_time = (double)start_time / (double)li.QuadPart;
#else	
	struct timeval tv;
	gettimeofday(&tv, NULL);
	unsigned long long end_time = tv.tv_sec * 1000000ULL + tv.tv_usec;
	start_time = end_time - start_time;
	ellapsed_time = (double)start_time / 1000000.0; 
#endif	
    return ellapsed_time;
}

