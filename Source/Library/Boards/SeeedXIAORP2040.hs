unit Board // Seeed XIAO RP2040
{
    #define SEEED_XIAO_RP2040

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040

    #define BOARD_HAS_NEOPIXEL
    #define BOARD_HAS_NEOPIXEL_POWER
    #define BOARD_HAS_LED
    #define BOARD_HAS_RGBLED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_NO_I2C0
    #define BOARD_HAS_NO_SPI1
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 17;
    const byte BuiltInLEDR = 17;
    const byte BuiltInLEDG = 16;
    const byte BuiltInLEDB = 25;
    const byte BuiltInNeoPixel = 12;
    const byte BuiltInNeoPixelLength = 1;
    const byte BuiltInNeoPixelPower = 11;

    const byte BuiltInA0 = 26;
    const byte BuiltInA1 = 27;
    const byte BuiltInA2 = 28;
    const byte BuiltInA3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA1 = 6;
    const byte I2CSCL1 = 7;

    const byte SPI0Tx = 3;
    const byte SPI0Rx = 4;
    const byte SPI0SCK = 2;
    const byte SPI0SS = 31;

    const byte UART1Tx = 0;
    const byte UART1Rx = 1;

    const byte D0 = 26;  // UART1Tx
    const byte D1 = 27;  // UART1Rx
    const byte D2 = 28;  // SPI0SCK
    const byte D3 = 29;  // SPI0Tx
    const byte D4 = 6;   // SPI0Rx
    const byte D5 = 7;
    const byte D6 = 0;   // I2CSDA1
    const byte D7 = 1;   // I2CSCL1
    const byte D8 = 2;
    const byte D9 = 4;
    const byte D10 = 3;

    uses "/Source/Library/MCU"
}
