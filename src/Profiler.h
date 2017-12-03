#pragma once
#include "mytypes.h"
#include "Array.h"
#include "Timer.h"

#define ENABLE_PROFILING 0

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

#if ENABLE_PROFILING

#define INIT_PROFILER()   initProfiler()
#define DELETE_PROFILER() deleteProfiler()
#define EXPORT_JSON(file) g_prof->exportJson(file)
#define CPU_SAMPLE(name)  CpuSample smp(name)
#define CPU_REPORT()      smp.report()

#else

#define INIT_PROFILER() 
#define DELETE_PROFILER() 
#define EXPORT_JSON(file) 
#define CPU_SAMPLE(name)  
#define CPU_REPORT()      

#endif