unit Board // Cytron Maker Pi RP2040
{
    #define CYTRON_MAKER_PI_RP2040

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040

    // Note: see Attribution.md (in the same folder as this file)

    #define BOARD_HAS_NEOPIXEL
    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_NO_SPI0
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 3;
    const byte BuiltInNeoPixel = 18;
    const byte BuiltInNeoPixelLength = 2;

    const byte A0 = 26;
    const byte A1 = 27;
    const byte A2 = 28;
    const byte A3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 16;
    const byte I2CSCL0 = 17;
    const byte I2CSDA1 = 2;
    const byte I2CSCL1 = 3;

    const byte SPI1Tx = 12;
    const byte SPI1Rx = 11;
    const byte SPI1SCK = 10;
    const byte SPI1SS = 15;

    const byte UART1Tx = 0;
    const byte UART1Rx = 1;
    const byte UART2Tx = 4;
    const byte UART2Rx = 5;


    uses "/Source/Library/MCU"

    string BoardName { get { return "Cytron Maker Pi RP2040"; } }

}
