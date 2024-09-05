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
    uses "/Source/System/Runtime"
    uses "/Source/System/IO"
    
    uses "GPIO"
    uses "SPI"
#ifdef BITBANG_I2C
    uses "/Source/Minimal/I2C"
#else  
  #if defined(BOARD_HAS_I2C)
    uses "Wire"
  #endif
#endif
    uses "SD"
    uses "Timer"

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
    
    Tone(byte pin, uint frequency, uint duration) library;
    Tone(byte pin, uint frequency)
    {
        Tone(pin, frequency, 0);
    }
    NoTone(byte pin) library;
    
    bool AttachToPin(byte pin, PinISRDelegate gpioISR, PinStatus status) library;
    
    bool InterruptsEnabled { get library; set library; }

#if defined(MCU_BOARD_RP2040) || defined(MCU_BOARD_RP2350)
    Reboot(bool bootsel) library;
    
    // Use these two APIs to keep an eye on the health of your MCU memory:
    long HeapFree() library;  // Is there a leak in the system level stuff? 
    long StackFree() library; // Are we close to the stack limit?

#if defined(MCU_BOARD_RP2040)        
    enum RPClockSpeed // enum so that tested good values are obvious:
    {
        Slow48       =  48,   // have not managed to reliably go slower than this
        Slow50       =  50,   
        Default      = 133,   // default for RP2040
        Overclock240 = 240,   // overclock sets clock to 240 MHz
        Overclock250 = 250,   // overclock sets clock to 250 MHz
        //Overclock266 = 266,   
        Overclock270 = 270,   // overclock sets clock to 270 MHz
        //Overclock275 = 275, // doesn't work on USB voltage
        //Overclock300 = 300, // doesn't work on USB voltage
    }
#endif
#if defined(MCU_BOARD_RP2350)        
    enum RPClockSpeed // enum so that tested good values are obvious:
    {
        Slow48       =  48,   // have not managed to reliably go slower than this
        Slow50       =  50,   
        Slow133      = 133,   // emulate RP2040 default
        Default      = 150,   // default for RP2350
        Overclock300 = 300,   // overclock sets clock to 300 MHz
        Overclock320 = 320,   // overclock sets clock to 300 MHz
    }
#endif    
    RPClockSpeed ClockSpeed { get library; set library; }
#endif

}
