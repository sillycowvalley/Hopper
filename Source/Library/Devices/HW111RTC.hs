unit RTCDevice
{
    uses "/Source/Library/RTCs/DS1307Driver"
    
    bool Begin()
    {
        return RTCDriver.begin(Wire.DefaultI2CController, Wire.DefaultI2CSDAPin, Wire.DefaultI2CSCLPin, 0x68);
    }
    
    bool Begin(byte i2cController, byte sdaPin, byte sclPin, byte address)
    {
        return RTCDriver.begin(i2cController, sdaPin, sclPin, address);
    }
    
}
