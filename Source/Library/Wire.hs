unit Wire
{
#if defined(BOARD_HAS_NO_I2C0) || defined(BOARD_I2C1_IS_DEFAULT)
    const byte DefaultI2CController = 1;
    const byte DefaultI2CSDAPin     = Board.I2CSDA1;
    const byte DefaultI2CSCLPin     = Board.I2CSCL1;
#else
    const byte DefaultI2CController = 0;
    const byte DefaultI2CSDAPin     = Board.I2CSDA0;
    const byte DefaultI2CSCLPin     = Board.I2CSCL0;
#endif

    bool Initialize()
    {
        return Initialize(DefaultI2CController, DefaultI2CSDAPin, DefaultI2CSCLPin);
    }
    bool Initialize(byte i2cController, byte sdaPin, byte sclPin)
    {
        bool success;
        loop
        {
#if defined(MCU_BOARD_RP2040)
            // While all of these combinations may not be valid,
            // they are the only possible superset.
            switch (i2cController)
            {
                case 0:
                {
#if defined(BOARD_HAS_NO_I2C0)
                    break;
#else
                    if ((sdaPin == 0) && (sclPin == 0)) 
                    { 
                        sdaPin = Board.I2CSDA0; 
                        sclPin = Board.I2CSCL0;
                    }
                    if (    ((sdaPin == Board.I2CSDA0) && (sclPin == Board.I2CSCL0))
                         || ((sdaPin == 0) && (sclPin == 1))
                         || ((sdaPin == 4) && (sclPin == 5))
                         || ((sdaPin == 8) && (sclPin == 9))
                         || ((sdaPin == 12) && (sclPin == 13))
                         || ((sdaPin == 16) && (sclPin == 17))
                         || ((sdaPin == 20) && (sclPin == 21))
                       ) { } else { break; }
#endif                     
                }
                case 1:
                {
#if defined(BOARD_HAS_NO_I2C1)
                    break;
#else
                    if ((sdaPin == 0) && (sclPin == 0)) 
                    { 
                        sdaPin = Board.I2CSDA1; 
                        sclPin = Board.I2CSCL1;
                    }
                    if (    ((sdaPin == Board.I2CSDA1) && (sclPin == Board.I2CSCL1))
                         || ((sdaPin == 2) && (sclPin == 3))
                         || ((sdaPin == 6) && (sclPin == 7))
                         || ((sdaPin == 10) && (sclPin == 11))
                         || ((sdaPin == 14) && (sclPin == 15))
                         || ((sdaPin == 18) && (sclPin == 19))
                         || ((sdaPin == 26) && (sclPin == 27))
                       ) { } else { break; }
#endif
                }
                default:
                {
                    break;
                }
            }
#endif
            Configure(i2cController, sdaPin, sclPin);
            success = Begin(i2cController);
            break;
        } // loop'
        return success;
    }
    
    // I2C APIs : this first ones assume controller 0 on default pins:
    bool Begin()  library;
    BeginTx(byte address)  library;
    byte EndTx()  library;
    Write(byte data) library;
    
    // These APIs support more than one I2C controller:
    bool Begin(byte i2cController)  library;
    BeginTx(byte i2cController, byte address)  library;
    byte EndTx(byte i2cController)  library;
    Write(byte i2cController, byte data) library;
    Write(byte i2cController, byte[] data, uint startIndex, uint length) library;
    
    byte RequestFrom(byte i2cController, byte address, byte bytes) library;
    byte Read(byte i2cController) library;
    
    Configure(byte i2cController, byte sdaPin, byte sclPin) library;
}
