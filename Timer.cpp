#include "Timer.h"
#include <windows.h>
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
    LARGE_INTEGER perfCount;
    QueryPerformanceCounter(&perfCount);
    start_time = perfCount.QuadPart;
}

void Timer::stopTimer()
{
    LARGE_INTEGER perfCount;
    QueryPerformanceCounter(&perfCount);
    unsigned long long end_time = perfCount.QuadPart;

    start_time = end_time - start_time;
}

void Timer::printTimeEllapsed()
{
    LARGE_INTEGER li;
    QueryPerformanceFrequency(&li);

    double ellapsed_time = (double)start_time / (double)li.QuadPart;
    printf("Time ellapsed: %lf seconds", ellapsed_time);
}
