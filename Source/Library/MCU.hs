unit MCU
{

#if !defined(SERIAL_CONSOLE)
  #define SERIAL_CONSOLE // for IO.hs if there is no alternative screen (like an LCD for example)
#endif

#if !defined(MCU)
  #define MCU           // for correct versions of System APIs (like Time.Delay(..) for example)
#endif

    uses "/Source/System/System"
    uses "/Source/System/IO"
    uses "/Source/Library/GPIO"
    uses "/Source/Library/Wire"
    uses "/Source/Library/SPI"

#if defined(CHALLENGER_RP2040_WIFI) || defined(RP2040_PICOW)    
    uses "/Source/System/WiFi"
    uses "/Source/Library/HttpClient"
    uses "/Source/Library/HttpServer"
#endif
    
#if defined(CHALLENGER_RP2040_WIFI) || defined(WAVESHARE_RP2040_ONE) || defined(WAVESHARE_RP2040_MATRIX) || defined(ADAFRUIT_FEATHER_RP2040) || defined(ADAFRUIT_METRO_RP2040) || defined(SPARKFUN_THING_PLUS_RP2040) || defined(SEEED_XIAO_RP2040)
    uses "/Source/Library/NeoPixel"
#endif

#if defined(RP2040_PICO) || defined(RP2040_PICOW) || defined(ADAFRUIT_FEATHER_RP2040) || defined(WAVESHARE_RP2040_MATRIX) || defined(CHALLENGER_RP2040_WIFI) || defined(PIMORONI_TINY2040) || defined(WAVESHARE_RP2040_ONE) || defined(SEEED_XIAO_RP2040) || defined(ARDUINO_NANO_RP2040) || defined(WEMOS_D1_MINI) || defined(ADAFRUIT_METRO_RP2040) || defined(SPARKFUN_THING_PLUS_RP2040)
    flags PinModeOption
    {
        Input         = 0x00,
        Output        = 0x01,
        InputPullup   = 0x02,
        InputPulldown = 0x03,
    }
#endif
            
#ifdef ARDUINO_NANO_ESP32    
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
