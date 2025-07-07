program Blink
{
    uses "/Source/Library/Boards/Challenger2350WiFi6Ble5"
    //uses "/Source/Library/Boards/PimoroniPicoPlus2W"
    //uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    //uses "/Source/Library/Boards/PimoroniTiny2350"
    
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
