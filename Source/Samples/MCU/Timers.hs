program TimerDemo
{
    #define RP2040_PICOW
    uses "/Source/Library/MCU"
    
    const byte alarmPin = 16;
    const byte timerPin = 17;
    long start;
    uint timerCount;
    uint alarmIDToCancel;
    
    fastTimerCallBack(uint timerID)
    {
        LED = !LED;
    }
    timerCallBack(uint timerID)
    {
        DigitalWrite(timerPin, true);
        timerCount++;
        Write("Timer: " + timerID.ToString() + " : " + timerCount.ToString() + " " + (Millis-start).ToString() + "ms");
        Delay(500);
        WriteLn(" .. done.");
        DigitalWrite(timerPin, false);
    }
    
    alarmCallBack(uint alarmID)
    {
        DigitalWrite(alarmPin, true);
        WriteLn("  Alarm: " + alarmID.ToString() + " " + (Millis-start).ToString() + "ms");
        Timer.Cancel(alarmIDToCancel);
        WriteLn("  Cancel Alarm: " + alarmIDToCancel.ToString());
        Delay(1000);
        DigitalWrite(alarmPin, false);
    }
    
    {
        LED = false; 
        MCU.PinMode(alarmPin, PinModeOption.Output); 
        MCU.DigitalWrite(alarmPin, false);
        MCU.PinMode(timerPin, PinModeOption.Output); 
        MCU.DigitalWrite(timerPin, false);
        
        TimerISRDelegate
        
        timerISRDelegate = fastTimerCallBack;
        uint timerID = Timer.Start(long(100), timerISRDelegate);
        WriteLn("Started 100us timer: " + timerID.ToString());
        
        timerISRDelegate = timerCallBack;
        start = Millis;
        timerID = Timer.Start(2000, timerISRDelegate);
        WriteLn("Started 2000ms timer: " + timerID.ToString());
        
        
        timerISRDelegate = alarmCallBack;
        uint alarmID = Timer.Alarm(10000, timerISRDelegate);
        WriteLn("Started 5000ms alarm: " + alarmID.ToString());
        
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
        }
    }
}
