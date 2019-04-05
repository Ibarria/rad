#include "Profiler.h"
#include "os.h"
#include <stdio.h>

Profiler *g_prof = nullptr;

Profiler::Profiler()
{
    entries.resize(1000);
    
    Timer t;
    t.startTimer();
    first_timestamp = t.getStartTime();
}


Profiler::~Profiler()
{
}

void Profiler::record(u64 start, u64 duration, u64 pid, u64 tid, const char * name)
{
    Entry &en = entries.getNextFree();
    en.start_timestamp = start - first_timestamp;
    en.duration = duration;
    en.pid = pid;
    en.tid = tid;
    en.name = name;
}

void Profiler::exportJson(const char * filename)
{
    FILE *f = fopen(filename, "w+");
    fprintf(f, "{\n\"traceEvents\": [\n");
    for (u32 i = 0; i < entries.size(); i++) {
        auto &ev = entries[i];
        fprintf(f, "{ \"pid\":%" U64FMT "d, \"tid\":%" U64FMT "d, \"ts\":%" U64FMT "d, " \
            "\"dur\":%" U64FMT "d, \"ph\":\"X\", \"name\":\"%s\", \"args\": {\"ms\":%f } }",
            ev.pid, ev.tid, ev.start_timestamp, ev.duration, ev.name, (double)ev.duration/1000.0);
        if (i < entries.size() - 1) {
            fprintf(f, ",");
        }
        fprintf(f, "\n");        
    }
    fprintf(f, "]\n}\n");
	fclose(f);
}


/*

{
"traceEvents": [
{ "pid":1, "tid":1, "ts":87705, "dur":956189, "ph":"X", "name":"Jambase", "args":{ "ms":956.2 } },
{ "pid":1, "tid":1, "ts":128154, "dur":75867, "ph":"X", "name":"SyncTargets", "args":{ "ms":75.9 } },
{ "pid":1, "tid":1, "ts":546867, "dur":121564, "ph":"X", "name":"DoThings", "args":{ "ms":121.6 } }
],
"meta_user": "aras",
"meta_cpu_count": "8"
}

*/

void initProfiler()
{
    g_prof = new Profiler();
}

void deleteProfiler()
{
    delete g_prof;
    g_prof = nullptr;
}

CpuSample::~CpuSample()
{
    if (!reported) {
        report();
    }
}

void CpuSample::report()
{
    u64 timestamp = timer.getStartTime();
    u64 duration = timer.stopTimerUs();

    g_prof->record(timestamp, duration,
        osGetCurrentProcessId(), osGetCurrentThreadId(), name);

    reported = true;
}
