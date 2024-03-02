unit BoardDevice
{

#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/SparkfunThingPlusRP2040" // avoid modifying the automatically generated Board file
#endif

    // TODO: Flash Memory - 16MB in a W25Q128JVPIM chip
    
    bool Begin()
    {
        bool success;
        loop
        {
            // https://learn.sparkfun.com/tutorials/rp2040-thing-plus-hookup-guide/hardware-overview
            // Settings for Hopper SD unit:
            SD.SPIController = 1;
            SD.ClkPin = Board.SPI1SCK;
            SD.TxPin  = Board.SPI1Tx;
            SD.RxPin  = Board.SPI1Rx;
            SD.CSPin  = Board.SPI1SS;
            
            success = true;
            break;
        }
        return success;
    }

}
