#pragma once
class Timer
{
    unsigned long long start_time;
public:
    Timer();
    ~Timer();
    void startTimer();
    void stopTimer();
    void printTimeEllapsed();
};

