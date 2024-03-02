program Mount
{
    uses "/Source/Library/Boards/PiPicoW"
    
    {
        SD.SPIController = 1;
        SD.ClkPin = 10;
        SD.TxPin  = 11;
        SD.RxPin  = 12;
        SD.CSPin  = 9;
        if (SD.Mount())
        {
            WriteLn("Successfully mounted SD card at '/sd/'");
        }
        else
        {
            WriteLn("Failed to mount SD card");
        }
    }
}
