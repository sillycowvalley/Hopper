program TimerDemo
{
    uses "/Source/Library/Boards/Pi"
    
    const byte alarmPin = 16;
    const byte timerPin = 17;
    
    long start;
    uint timerCount;
    uint alarmIDToCancel;
    
    fastTimerCallBack(uint timerID)
    {
        Write(".");
    }
    
    timerCallBack(uint timerID)
    {
        timerCount++;
        Write("Timer: " + timerID.ToString() + " : " + timerCount.ToString() + " " + (Millis-start).ToString() + "ms");
        Delay(500);
        WriteLn(" .. done.");
    }
    
    alarmCallBack(uint alarmID)
    {
        WriteLn("  Alarm: " + alarmID.ToString() + " " + (Millis-start).ToString() + "ms");
        Timer.Cancel(alarmIDToCancel);
        Write("  Cancel Alarm: " + alarmIDToCancel.ToString());
        Delay(500);
        WriteLn(" .. done.");
    }
    
    Hopper()
    {
        
        TimerISRDelegate timerISRDelegate;
        timerISRDelegate = fastTimerCallBack;
        uint fastTimerID = Timer.Start(100, timerISRDelegate);
        WriteLn("Started 100ms timer: " + fastTimerID.ToString());
        
        timerISRDelegate = timerCallBack;
        start = Millis;
        uint timerID = Timer.Start(2000, timerISRDelegate);
        WriteLn("Started 2000ms timer: " + timerID.ToString());
        
        timerISRDelegate = alarmCallBack;
        uint alarmID = Timer.Alarm(10000, timerISRDelegate);
        WriteLn("Started 10000ms alarm: " + alarmID.ToString());
        
        alarmIDToCancel = Timer.Alarm(15000, timerISRDelegate);
        WriteLn("Started 15000ms alarm: " + alarmIDToCancel.ToString());
        
        bool cancelled;
        loop
        {
            if ((timerCount >= 10) && !cancelled)
            {
                Timer.Stop(timerID);
                WriteLn("Stop Timer: " + timerID.ToString());
                cancelled = true;
            }
            if (IO.IsAvailable)
            {
                break;
            }
        }
        if (!cancelled)
        {
            Timer.Stop(timerID);
            WriteLn("Stop Timer: " + timerID.ToString());
        }
        WriteLn("Stop Timer: " + fastTimerID.ToString());
        Timer.Stop(fastTimerID);
        Delay(3000);
        WriteLn("Exit");
    }
}
