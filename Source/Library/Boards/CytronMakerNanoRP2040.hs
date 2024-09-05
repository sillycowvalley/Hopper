unit Board // Cytron Maker Nano RP2040
{
    #define CYTRON_MAKER_NANO_RP2040

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP

    // Note: see Attribution.md (in the same folder as this file)

    #define BOARD_HAS_NEOPIXEL
    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_NO_SPI1
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 2;
    const byte BuiltInNeoPixel = 11;
    const byte BuiltInNeoPixelLength = 2;

    const byte A0 = 26;
    const byte A1 = 27;
    const byte A2 = 28;
    const byte A3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 0;
    const byte I2CSCL0 = 1;
    const byte I2CSDA1 = 26;
    const byte I2CSCL1 = 27;

    const byte SPI0Tx = 19;
    const byte SPI0Rx = 16;
    const byte SPI0SCK = 18;
    const byte SPI0SS = 17;

    const byte UART1Tx = 0;
    const byte UART1Rx = 1;


    uses "/Source/Library/MCU"

    string BoardName { get { return "Cytron Maker Nano RP2040"; } }

}
