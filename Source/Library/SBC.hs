unit MCU
{

#if !defined(SBC_BOARD_DEFINED)
    #error "Find your board under /Source/Library/Boards, add 'uses' for it in your program"
#endif

#if !defined(SBC)
  #define SBC            // for correct versions of System APIs (like Time.Delay(..) for example)
#endif

    uses "/Source/System/System"
    uses "/Source/System/IO"
    
    uses "Timer"
    uses "GPIO"

#if defined(BOARD_HAS_WIFI)
    uses "/Source/System/WiFi"
    uses "WebClient"
    uses "WebServer"
#endif
    
#if defined(BOARD_HAS_NEOPIXEL)
    uses "NeoPixel"
#endif

    flags PinModeOption
    {
        Input         = 0x00,
        Output        = 0x01,
        InputPullup   = 0x02,
        InputPulldown = 0x03,
    }
    
    enum PinStatus
    {
        Low = 0,
        High = 1,
        Change = 2,
        Falling = 3,
        Rising = 4,
    }
    delegate PinISRDelegate(byte pin, PinStatus status);

    PinMode(byte pin, PinModeOption pinMode) library;
    bool DigitalRead(byte pin) library;
    DigitalWrite(byte pin, bool value) library;
    uint AnalogRead(byte pin) library;
    AnalogWrite(byte pin, uint value) library;
    AnalogWriteResolution(byte bits) library;
    
    bool AttachToPin(byte pin, PinISRDelegate gpioISR, PinStatus status) library;
    
    bool InterruptsEnabled { get library; set library; }

}
