program Blink
{
    //uses "/Source/Library/Boards/PiPico2"
    //uses "/Source/Library/Boards/PimoroniPicoPlus2W"
    uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    
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

