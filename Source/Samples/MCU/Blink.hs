program Blink
{
    //uses "/Source/Library/Boards/PiPico2"
    //uses "/Source/Library/Boards/PimoroniPicoPlus2W"
    //uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    uses "/Source/Library/Boards/PimoroniTiny2350"
    
    Hopper()
    {
        bool on;
        MCU.PinMode(GP6, MCU.PinModeOption.Output);
        
        loop
        {
            Delay(500);
            
            LED = !LED;
            
            DigitalWrite(GP6, on);
            on = !on;
            
            WriteLn((Time.Seconds).ToString() + " seconds");
        }
    }
}

