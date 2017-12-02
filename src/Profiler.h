#pragma once
#include "mytypes.h"
#include "Array.h"
#include "Timer.h"
/*
    Very simple profiler class for now, we store the time samples
    and at the end of the program we provide the summary. 

    In the future, extend this to support multiple threads, 
    and maybe an extra thread to write the profiling data to
    disk as it is being created

*/

struct Entry {
    u64 start_timestamp;
    u64 duration;
    u64 pid;
    u64 tid;
    const char *name;
};

class Profiler
{
    u64 first_timestamp;

    Array<Entry> entries;

public:
    Profiler();
    ~Profiler();
    void record(u64 start, u64 duration, u64 pid, u64 tid, const char *name);
    void exportJson(const char *filename);
};

extern Profiler *g_prof;

void initProfiler();
void deleteProfiler();

class CpuSample
{
    Timer timer;
    const char *name;
    bool reported;
public:
    CpuSample(const char *n) { timer.startTimer(); name = n; reported = false; }
    ~CpuSample();

    void report();
};