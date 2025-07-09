program TimerDemo
{
    uses "/Source/Library/Boards/Pi"
    
    const byte alarmPin = 4;
    const byte timerPin = 5;
    const byte timerPin2 = 6;
    
    long start;
    uint timerCount;
    uint alarmIDToCancel;
    
    fastTimerCallBack(uint timerID)
    {
        DigitalWrite(timerPin, true);
        Delay(100);
        DigitalWrite(timerPin, false);
    }
    
    timerCallBack(uint timerID)
    {
        DigitalWrite(timerPin2, true);
        timerCount++;
        Write("Timer: " + timerID.ToString() + " : " + timerCount.ToString() + " " + (Millis-start).ToString() + "ms");
        Delay(500);
        WriteLn(" .. done.");
        DigitalWrite(timerPin2, false);
    }
    
    alarmCallBack(uint alarmID)
    {
        DigitalWrite(alarmPin, true);
        WriteLn("  Alarm: " + alarmID.ToString() + " " + (Millis-start).ToString() + "ms");
        Timer.Cancel(alarmIDToCancel);
        Write("  Cancel Alarm: " + alarmIDToCancel.ToString());
        Delay(500);
        WriteLn(" .. done.");
        DigitalWrite(alarmPin, false);
    }
    
    Hopper()
    {
        PinMode(alarmPin, PinModeOption.Output);
        PinMode(timerPin, PinModeOption.Output);
        PinMode(timerPin2, PinModeOption.Output);
        
        TimerISRDelegate timerISRDelegate;
        timerISRDelegate = fastTimerCallBack;
        uint fastTimerID = Timer.Start(250, timerISRDelegate);
        WriteLn("Started 250ms timer: " + fastTimerID.ToString());
        
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
        DigitalWrite(timerPin, false);
        DigitalWrite(alarmPin, false);
        DigitalWrite(timerPin2, false);
        WriteLn("Exit");
    }
}
