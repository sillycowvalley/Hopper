unit MCU
{

#define PORTABLE      // use Hopper versions of runtime library functions (minimal platform requirements)
#define SERIALCONSOLE // for IO.hs if there is no alternative screen (like an LCD for example)

#define MCU           // for correct versions of System APIs (like Time.Delay(..) for example)

    uses "/Source/System/System"
    uses "/Source/System/IO"
    uses "/Source/System/GPIO"
    
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
#ifdef RP2040
    flags PinModeOption
    {
        Input         = 0x00,
        Output        = 0x01,
        InputPullup   = 0x02,
        InputPulldown = 0x03,
    }
#endif  
#ifdef SEEEDRP2040
    flags PinModeOption
    {
        Input         = 0x00,
        Output        = 0x01,
        InputPullup   = 0x02,
        InputPulldown = 0x03,
    }
#endif
#ifdef ARDUINONANORP2040
    flags PinModeOption
    {
        Input         = 0x00,
        Output        = 0x01,
        InputPullup   = 0x02,
        InputPulldown = 0x03,
    }
#endif
#ifdef WEMOSD1MINI
    flags PinModeOption
    {
        Input         = 0x00,
        Output        = 0x01,
        InputPullup   = 0x02,
        InputPulldown = 0x03,
    }
#endif

    PinMode(byte pin, PinModeOption pinMode) system;
    bool DigitalRead(byte pin) system;
    DigitalWrite(byte pin, bool value) system;
    
}
