unit Board // Sparkfun ThingPlus RP2040
{
    #define SPARKFUN_THINGPLUSRP2040

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP

    // Note: see Attribution.md (in the same folder as this file)

    #define BOARD_HAS_NEOPIXEL
    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 25;
    const byte BuiltInNeoPixel = 8;
    const byte BuiltInNeoPixelLength = 1;

    const byte A0 = 26;
    const byte A1 = 27;
    const byte A2 = 28;
    const byte A3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 16;
    const byte I2CSCL0 = 17;
    const byte I2CSDA1 = 6;
    const byte I2CSCL1 = 7;

    const byte SPI0Tx = 3;
    const byte SPI0Rx = 4;
    const byte SPI0SCK = 2;
    const byte SPI0SS = 31;
    const byte SPI1Tx = 15;
    const byte SPI1Rx = 12;
    const byte SPI1SCK = 14;
    const byte SPI1SS = 9;

    const byte UART1Tx = 0;
    const byte UART1Rx = 1;

    const byte GP0 = 0;   // UART1Tx
    const byte GP1 = 1;   // UART1Rx
    const byte GP2 = 6;   // SPI0SCK
    const byte GP3 = 7;   // SPI0Tx
    const byte GP6 = 8;   // I2CSDA1
    const byte GP7 = 22;  // I2CSCL1
    const byte GP8 = 21;  // BuiltInNeoPixel
    const byte GP9 = 20;  // SPI1SS
    const byte GP10 = 19;
    const byte GP11 = 18;
    const byte GP12 = 17; // SPI1Rx
    const byte GP13 = 16;
    const byte GP20 = 4;
    const byte GP19 = 3;
    const byte GP18 = 2;
    const byte GP26 = 26; // A0
    const byte GP27 = 27; // A1
    const byte GP28 = 28; // A2
    const byte GP29 = 29; // A3

    uses "/Source/Library/MCU"

    string BoardName { get { return "Sparkfun ThingPlus RP2040"; } }

}
