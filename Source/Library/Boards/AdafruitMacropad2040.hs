unit Board // Adafruit Macropad2040
{
    #define ADAFRUIT_MACROPAD2040

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040

    // Note: see Attribution.md (in the same folder as this file)

    #define BOARD_HAS_NEOPIXEL
    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_NO_I2C1
    #define BOARD_HAS_NO_SPI0
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 13;
    const byte BuiltInNeoPixel = 19;
    const byte BuiltInNeoPixelLength = 12;

    const byte A0 = 26;
    const byte A1 = 27;
    const byte A2 = 28;
    const byte A3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 20;
    const byte I2CSCL0 = 21;

    const byte SPI1Tx = 27;
    const byte SPI1Rx = 28;
    const byte SPI1SCK = 26;
    const byte SPI1SS = 31;


    uses "/Source/Library/MCU"

    string BoardName { get { return "Adafruit Macropad2040"; } }

}
