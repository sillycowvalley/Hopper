unit MCU
{

#define PORTABLE      // use Hopper versions of runtime library functions (minimal platform requirements)
#define SERIALCONSOLE // for IO.hs if there is no alternative screen (like an LCD for example)

#define MCU           // for correct versions of System APIs (like Time.Delay(..) for example)

    uses "/Source/System/System"
    uses "/Source/System/IO"
    uses "/Source/System/GPIO"
    
    enum PinModeOption
    {
        Input,
        Output,
        InputPullup,
    }
    
    PinMode(byte pin, PinModeOption pinMode) system;
    bool DigitalRead(byte pin) system;
    DigitalWrite(byte pin, bool value) system;
    
}
