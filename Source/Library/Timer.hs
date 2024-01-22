unit Timer
{
    delegate TimerISRDelegate(uint timerID);
    
    // Timers: repeating
    uint Start(uint msInterval, TimerISRDelegate timerISR) library;
    uint Start(long msInterval, TimerISRDelegate timerISR) library;
    Stop(uint timerID) library;
    
    // Alarms: one-shot
    uint Alarm(uint msInterval, TimerISRDelegate timerISR) library;
    uint Alarm(long msInterval, TimerISRDelegate timerISR) library;
    Cancel(uint alarmID) library;
}
