program Blink
{
    //uses "/Source/Library/Boards/Pi"
    //uses "/Source/Library/Boards/Challenger2350WiFi6Ble5"
    //uses "/Source/Library/Boards/PimoroniPicoPlus2W"
    //uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    //uses "/Source/Library/Boards/PimoroniTiny2350"
    uses "/Source/Library/Boards/MECB6502"
    //uses "/Source/Library/Boards/Hopper6502"
    
#ifndef MCU    
    //uses "/Source/System/Screen"
#endif
    
    Hopper()
    {
        loop
        {
            Delay(500);
            LED = !LED;
            WriteLn((Time.Seconds).ToString() + " seconds");
            if (IO.IsAvailable)
            {
                LED = false;
                break;
            }
        }
    }
}
