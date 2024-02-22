unit RTCDevice
{

#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/AdafruitFeather" // good default since it is a FeatherWing
#endif

    uses "/Source/Library/RTCs/DS3231Driver"
    
    bool Begin()
    {
        // Wire defaults should be correct since it is a FeatherWing:
        return RTCDriver.begin(Wire.DefaultI2CController, Wire.DefaultI2CSDAPin, Wire.DefaultI2CSCLPin, 0x68);
    }
    
}
