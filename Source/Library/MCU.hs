unit MCU
{

#if !defined(MCU_BOARD_DEFINED)
    #error "Find your board under /Source/Library/Boards, add 'uses' for it in your program"
#endif

#if !defined(SERIAL_CONSOLE)
  #define SERIAL_CONSOLE // for IO.hs if there is no alternative screen (like an LCD for example)
#endif

#if !defined(MCU)
  #define MCU            // for correct versions of System APIs (like Time.Delay(..) for example)
#endif

    uses "/Source/System/System"
    uses "/Source/System/IO"
    uses "/Source/Library/GPIO"
    uses "/Source/Library/SPI"
#if defined(BOARD_HAS_I2C)
    uses "/Source/Library/Wire"
#endif
    //uses "/Source/Library/SD"
    uses "/Source/Library/Timer"

#if defined(BOARD_HAS_WIFI)
    uses "/Source/System/WiFi"
    uses "/Source/Library/WebClient"
    uses "/Source/Library/WebServer"
#endif
    
#if defined(BOARD_HAS_NEOPIXEL)
    uses "/Source/Library/NeoPixel"
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
    
    Reboot(bool bootsel) library;
    
    // Use these two APIs to keep an eye on the health of your MCU memory:
    long HeapFree() library;  // Is there a leak in the system level stuff? 
    long StackFree() library; // Are we close to the stack limit?
}
