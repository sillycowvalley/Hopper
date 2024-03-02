unit BoardDevice
{
#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/AdafruitMetro" // avoid modifying the automatically generated Board file
#endif
    
    bool Begin()
    {
        bool success;
        loop
        {
            // https://learn.adafruit.com/adafruit-metro-rp2040/sd-card-example
            // Settings for Hopper SD unit:
            SD.SPIController = 0;
            SD.ClkPin = Board.SPI0SCK;
            SD.TxPin  = Board.SPI0Tx;
            SD.RxPin  = Board.SPI0Rx;
            SD.CSPin  = Board.SPI0SS;
            
            success = true;
            break;
        }
        return success;
    }
    
}
