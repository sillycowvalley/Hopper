program Blink
{
    //uses "/Source/Library/Boards/PiPico2"
    uses "/Source/Library/Boards/PimoroniPicoPlus2W"
    
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

