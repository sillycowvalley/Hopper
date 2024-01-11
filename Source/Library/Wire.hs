unit Wire
{
    bool Initialize(byte i2cController, byte sdaPin, byte sclPin)
    {
        bool success;
        loop
        {
#if defined(RP2040_PICO) || defined(RP2040_PICOW)            // Pi Pico pin validation:
            switch (i2cController)
            {
                case 0:
                {
                    if ((sdaPin == 0) && (sclPin == 0)) { sdaPin = 4; } // zero is a legit value for SDA for controller 0
                    if (sclPin == 0) { sclPin = 5; }
                    if (    ((sdaPin == 0) && (sclPin == 1))
                         || ((sdaPin == 4) && (sclPin == 5))
                         || ((sdaPin == 8) && (sclPin == 9))
                         || ((sdaPin == 12) && (sclPin == 13))
                         || ((sdaPin == 16) && (sclPin == 17))
                         || ((sdaPin == 20) && (sclPin == 21))
                       ) { } else { break; }
                     
                }
                case 1:
                {
                    if (sdaPin == 0) { sdaPin = 26; }
                    if (sclPin == 0) { sclPin = 27; }
                    if (    ((sdaPin == 2) && (sclPin == 3))
                         || ((sdaPin == 6) && (sclPin == 7))
                         || ((sdaPin == 10) && (sclPin == 11))
                         || ((sdaPin == 14) && (sclPin == 15))
                         || ((sdaPin == 18) && (sclPin == 19))
                         || ((sdaPin == 26) && (sclPin == 27))
                       ) { } else { break; }
                }
                default:
                {
                    break;
                }
            }
#endif
#if defined(PIMORONI_TINY2040)
            switch (i2cController)
            {
                case 0:
                {
                    if ((sdaPin == 0) && (sclPin == 0)) { sdaPin = 4; } // zero is a legit value for SDA for controller 0
                    if (sclPin == 0) { sclPin = 5; }
                    if (    ((sdaPin == 0) && (sclPin == 1))
                         || ((sdaPin == 4) && (sclPin == 5))
                       ) { } else { break; }
                     
                }
                case 1:
                {
                    if (sdaPin == 0) { sdaPin = 26; }
                    if (sclPin == 0) { sclPin = 27; }
                    if (    ((sdaPin == 2) && (sclPin == 3))
                         || ((sdaPin == 6) && (sclPin == 7))
                         || ((sdaPin == 26) && (sclPin == 27))
                       ) { } else { break; }
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
    
    Configure(byte i2cController, byte sdaPin, byte sclPin) library;
}
