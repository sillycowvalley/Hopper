unit MCU
{
    
#if defined(RP2040_PICO) || defined(RP2040_PICOW) || defined(MAKER_NANO_RP2040) || defined(WAVESHARE_RP2040_PLUS) || defined(PIMORONI_PICOLIPO)
    #define PICO_PINS // Raspberry Pi Pico form factor and pins
#endif
#if defined(ADAFRUIT_FEATHER_RP2040) || defined(CHALLENGER_RP2040_WIFI) || defined(CHALLENGER_RP2040_SDRTC) || defined(SPARKFUN_THING_PLUS_RP2040)
    #define FEATHER_PINS // Adafruit Feather form factor and pins
#endif
#if defined(WAVESHARE_RP2040_MATRIX) || defined(ARDUINO_NANO_RP2040) ||  defined(ARDUINO_NANO_ESP32) || defined(ADAFRUIT_METRO_RP2040) || defined(WEMOS_D1_MINI) || defined(PIMORONI_TINY2040) || defined(WAVESHARE_RP2040_ONE) || defined(SEEED_XIAO_RP2040) 
    #define OTHER_PINS // non-standard form factor and pins
#endif
#if !defined(PICO_PINS) && !defined(FEATHER_PINS) && !defined(OTHER_PINS)

    #error "To use 'MCU' unit, define the target board at the start of your program (see comment below)"
    // Supported Boards (that have been tested):
    //
    // RP2040_PICO:
    //  - most common RP2040 generic target, fine choice if:
    //    - board doesn't have an RGB NeoPixel
    //    - board does not have WiFi
    //    - board has built-in LED on pin 25
    //  Boards that have been tested and work work with 'HopperPico.uf2' include:
    //  - Adafruit Metro RP2040 (ADAFRUIT_METRO_RP2040)
    //  - Adafruit QT Py (ADAFRUIT_QTPY)
    //  - Pimoroni Tiny 2040 (PIMORONI_TINY2040)
    //  - Pimoroni Pico Lipo (PIMORONI_PICOLIPO)
    //  - Raspberry Pi Pico  (RP2040_PICO)
    //  - Waveshare RP2040 Plus (WAVESHARE_RP2040_PLUS)
    //
    // RP2040_PICOW:
    //  - good target for Pico W clones where:
    //    - the WiFi coprocessor is the CYW43439
    //    - built in LED is on driven by this coprocessor
    //  Boards that have been tested and work with 'HopperPicoW.uf2' include:
    //  - Raspberry Pi Pico W  (RP2040_PICOW)
    //  - Arduino RP2040 Nano Connect (ARDUINO_NANO_RP2040)
    //
    // CHALLENGER_RP2040_WIFI:
    //  - WiFi coprocessor is ESP8285
    //  - NeoPixel support for built-in RGB LED
    //  Boards that have been tested and work with 'HopperChallengerWiFi.uf2' include:
    //  - Challenger RP2040 WiFi
    //  - Challenger NB RP2040 WiFi
    //
    // ARDUINO_NANO_ESP32:
    //  - for some reason the PinModeOption values are different to everyone else!
    //
    // Reasons use a non-generic RP2040_PICO board define:
    // 
    //   Non-standard pin configuration:
    //   - ADAFRUIT_METRO_RP2040 : built-in LED on pin 13
    //   - PIMORONI_TINY2040: built-in RGB LEDs on 18, 19 and 20
    //   - SEEED_RP2040: built-in RGB LEDs on 17, 16 and 25
    //   - ADAFRUIT_QTPY: A0..A3 pin order reversed compared to Pi Pico
    //   - ARDUINO_NANO_RP2040: built-in LED on pin 6, only A0..A1 (no A2 or A3)
    //
    //   I2C defaults and pin validation:
    //   - PIMORONI_TINY2040 : 2 controllers but a lot less valid pins
    //
    //   Pins and string lengths for built-in NeoPixels:
    //   - WAVESHARE_RP2040_MATRIX, WAVESHARE_RP2040_ONE
    //   - MAKER_NANO_RP2040
    //   - CHALLENGER_RP2040_WIFI
    //   - ADAFRUIT_FEATHER_RP2040, ADAFRUIT_METRO_RP2040
    //   - SPARKFUN_THING_PLUS_RP2040
    //   - SEEED_XIAO_RP2040
    //
    // Non-RP2040 boards that need more work/testing: 
    // - WEMOS_D1_MINI
    // - ARDUINO_NANO_ESP32
    
#endif

#if !defined(SERIAL_CONSOLE)
  #define SERIAL_CONSOLE // for IO.hs if there is no alternative screen (like an LCD for example)
#endif

#if !defined(MCU)
  #define MCU           // for correct versions of System APIs (like Time.Delay(..) for example)
#endif

#if defined(CHALLENGER_RP2040_WIFI) || defined(RP2040_PICOW) || defined(ARDUINO_NANO_RP2040)
    #define USES_WIFI
#endif

#if defined(CHALLENGER_RP2040_WIFI) || defined(WAVESHARE_RP2040_ONE) || defined(MAKER_NANO_RP2040) || defined(WAVESHARE_RP2040_MATRIX) || defined(ADAFRUIT_FEATHER_RP2040) || defined(ADAFRUIT_METRO_RP2040) || defined(SPARKFUN_THING_PLUS_RP2040) || defined(SEEED_XIAO_RP2040)
    #define USES_NEOPIXEL
#endif

    uses "/Source/System/System"
    uses "/Source/System/IO"
    uses "/Source/Library/GPIO"
    uses "/Source/Library/Wire"
    uses "/Source/Library/SPI"
    //uses "/Source/Library/SD"
    uses "/Source/Library/Timer"

#if defined(USES_WIFI)
    uses "/Source/System/WiFi"
    uses "/Source/Library/WebClient"
    uses "/Source/Library/WebServer"
#endif
    
#if defined(USES_NEOPIXEL)
    uses "/Source/Library/NeoPixel"
#endif



#if defined(PICO_PINS) || defined(FEATHER_PINS) || defined(OTHER_PINS) 

  #ifdef ARDUINO_NANO_ESP32
    // Why?
    flags PinModeOption
    {
        Input         = 0x01,
        Output        = 0x03,
        Pullup        = 0x04,
        Pulldown      = 0x08,
        InputPullup   = 0x05,
        InputPulldown = 0x09,
    }
  #else
    flags PinModeOption
    {
        Input         = 0x00,
        Output        = 0x01,
        InputPullup   = 0x02,
        InputPulldown = 0x03,
    }
  #endif
    
#endif

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
