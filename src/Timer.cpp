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

double Timer::stopTimer()
{
    LARGE_INTEGER perfCount;
    LARGE_INTEGER li;
    QueryPerformanceCounter(&perfCount);
    QueryPerformanceFrequency(&li);

    unsigned long long end_time = perfCount.QuadPart;
    start_time = end_time - start_time;
    double ellapsed_time = (double)start_time / (double)li.QuadPart;
    return ellapsed_time;
}

