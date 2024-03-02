unit RTCDevice
{

#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/Challenger2040SDRTC" // sneaky way of not modifying the automatically generated Board file
#endif

    uses "/Source/Library/RTCs/MCP79410Driver" 
    
    const byte IntPin = 25;
    const byte DetectPin = 13;
    
    bool SDPresent { get { return !DigitalRead(DetectPin); } }
    
    bool Begin()
    {
        bool success;
        loop
        {
            MCU.PinMode(DetectPin, PinModeOption.Input);
            
            // https://ilabs.se/challenger-rp2040-sd-rtc-datasheet/
            // Settings for Hopper SD unit:
            SD.SPIController = 1;
            SD.ClkPin = 10;
            SD.TxPin  = 11;
            SD.RxPin  = 12;
            SD.CSPin  = 9;
            
            if (!RTCDriver.begin(Wire.DefaultI2CController, Wire.DefaultI2CSDAPin, Wire.DefaultI2CSCLPin, 0x6F))
            {
                break;
            }
            success = true;
            break;
        }
        return success;
    }
    
}
