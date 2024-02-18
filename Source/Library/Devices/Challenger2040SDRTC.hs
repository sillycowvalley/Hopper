unit RTCDevice
{

#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/Challenger2040SDRTC" // sneaky way of not modifying the automatically generated Board file
#endif

    uses "/Source/Library/RTCs/MCP79410Driver"
    
    const byte IntPin = 25;
    
    bool Begin()
    {
        return RTCDriver.Begin(Wire.DefaultI2CController, Wire.DefaultI2CSDAPin, Wire.DefaultI2CSCLPin, 0x6F);
    }
    
}
