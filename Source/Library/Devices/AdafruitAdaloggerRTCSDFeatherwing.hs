unit RTCDevice
{

#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/AdafruitFeather" // good default since it is a FeatherWing
#endif

    uses "/Source/Library/RTCs/PCF8523Driver"
       
    bool Begin()
    {
        bool success;
        loop
        {
        
            SD.SPIController = 0;
            SD.ClkPin = Board.SPI0SCK;
            SD.TxPin  = Board.SPI0Tx;
            SD.RxPin  = Board.SPI0Rx;
            
            SD.CSPin  = Board.GP10;
            
            /* Don't fail if there is no SD card in the board:
            if (!SD.Mount())
            {
                break;
            }
            */
            
            // Wire defaults should be correct since it is a FeatherWing:
            if (!RTCDriver.begin(Wire.DefaultI2CController, Wire.DefaultI2CSDAPin, Wire.DefaultI2CSCLPin, 0x68))
            {
                break;
            }
            success = true;
            break;
        }
        return success;
    }
    
}
