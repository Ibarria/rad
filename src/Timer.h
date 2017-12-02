#pragma once
class Timer
{
    u64 start_time;
    double scale;
public:
    Timer();
    ~Timer();
    void startTimer();
    u64 getStartTime() { return start_time; }

    // Both functions are mutually exclusive, pick one
    u64 stopTimerUs();
    double stopTimer();
};

