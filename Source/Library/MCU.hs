unit MCU
{

#if !defined(PORTABLE)
  #define PORTABLE      // use Hopper versions of runtime library functions (minimal platform requirements)
#endif
#if !defined(SERIALCONSOLE)
  #define SERIALCONSOLE // for IO.hs if there is no alternative screen (like an LCD for example)
#endif

#if !defined(MCU)
  #define MCU           // for correct versions of System APIs (like Time.Delay(..) for example)
#endif

    uses "/Source/System/System"
    uses "/Source/System/IO"
    uses "/Source/Library/GPIO"
    uses "/Source/Library/Wire"

#if defined(RP2040PICO) || defined(RP2040PICOW) || defined(TINY2040) || defined(WAVESHARERP2040ONE) || defined(SEEEDRP2040) || defined(ARDUINONANORP2040) || defined(WEMOSD1MINI)
    flags PinModeOption
    {
        Input         = 0x00,
        Output        = 0x01,
        InputPullup   = 0x02,
        InputPulldown = 0x03,
    }
#endif
            
#ifdef ARDUINONANOESP32    
    flags PinModeOption
    {
        Input         = 0x01,
        Output        = 0x03,
        Pullup        = 0x04,
        Pulldown      = 0x08,
        InputPullup   = 0x05,
        InputPulldown = 0x09,
    }
#endif

    enum PinStatus
    {
        Low = 0,
        High = 1,
        Change = 2,
        Falling = 3,
        Rising = 4,
    }
    delegate ISRDelegate(byte pin, PinStatus status);

    PinMode(byte pin, PinModeOption pinMode) library;
    bool DigitalRead(byte pin) library;
    DigitalWrite(byte pin, bool value) library;
    uint AnalogRead(byte pin) library;
    AnalogWrite(byte pin, uint value) library;
    AnalogWriteResolution(byte bits) library;
    
    bool AttachToPin(byte pin, ISRDelegate gpioISR, PinStatus status) library;
}
