program Mount
{
    uses "/Source/Library/Boards/PiPico"
    
    {
#if defined(BOARD_HAS_NO_SPI0)
        SD.SPIController = 1;
        SD.ClkPin = Board.SPI1SCK;
        SD.TxPin  = Board.SPI1Tx;
        SD.RxPin  = Board.SPI1Rx;
        SD.CSPin  = Board.SPI1SS;
#else
        SD.SPIController = 0;
        SD.ClkPin = Board.SPI0SCK;
        SD.TxPin  = Board.SPI0Tx;
        SD.RxPin  = Board.SPI0Rx;
        SD.CSPin  = Board.SPI0SS;
#endif   
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
