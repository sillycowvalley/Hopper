program Blink
{
    uses "/Source/Library/Boards/PiPico"
    //uses "/Source/Library/Boards/Hopper6502"
    
    {
        loop
        {
            Delay(500);
            LED = !LED;
            WriteLn((Time.Seconds).ToString() + " seconds");
        }
    }
}

