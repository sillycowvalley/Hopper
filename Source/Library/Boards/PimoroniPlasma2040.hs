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

    const byte GP0 = 26;
    const byte GP1 = 27;
    const byte GP2 = 28;
    const byte GP3 = 29;
    const byte GP4 = 6;
    const byte GP5 = 7;
    const byte GP6 = 0;
    const byte GP7 = 1;
    const byte GP8 = 2;
    const byte GP9 = 4;
    const byte GP10 = 3;

    uses "/Source/Library/MCU"

    string BoardName { get { return "Pimoroni Plasma2040"; } }

}
