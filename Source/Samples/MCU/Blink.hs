program Blink
{
    uses "/Source/Library/Boards/PiPico2"
    
    Hopper()
    {
        loop
        {
            Delay(500);
            
            LED = !LED;
            
            WriteLn((Time.Seconds).ToString() + " seconds");
        }
    }
}

