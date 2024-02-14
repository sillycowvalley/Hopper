unit Board // Pimoroni Plasma2040
{
    #define PIMORONI_PLASMA2040

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040

    #define BOARD_HAS_NEOPIXEL
    #define BOARD_HAS_LED
    #define BOARD_HAS_RGBLED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_NO_I2C0
    #define BOARD_HAS_NO_SPI0
    #define BOARD_HAS_NO_SPI1
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 16;
    const byte BuiltInLEDR = 16;
    const byte BuiltInLEDG = 17;
    const byte BuiltInLEDB = 18;
    const byte BuiltInNeoPixel = 15;
    const byte BuiltInNeoPixelLength = 1;

    const byte BuiltInA0 = 26;
    const byte BuiltInA1 = 27;
    const byte BuiltInA2 = 28;
    const byte BuiltInA3 = 31;
    const byte ADCResolution = 12;

    const byte I2CSDA1 = 20;
    const byte I2CSCL1 = 21;

    const byte D0 = 26;
    const byte D1 = 27;
    const byte D2 = 28;
    const byte D3 = 29;
    const byte D4 = 6;
    const byte D5 = 7;
    const byte D6 = 0;
    const byte D7 = 1;
    const byte D8 = 2;
    const byte D9 = 4;
    const byte D10 = 3;

    uses "/Source/Library/MCU"
}
