unit Board // Adafruit Feather
{
    #define ADAFRUIT_FEATHER

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040
    #define FORMFACTOR_FEATHER

    // https://learn.adafruit.com/adafruit-feather-rp2040-pico/pinouts

    #define BOARD_HAS_NEOPIXEL
    #define BOARD_HAS_LED
    #define BOARD_I2C1_IS_DEFAULT
    #define BOARD_HAS_I2C
    #define BOARD_HAS_NO_SPI1
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 13;
    const byte BuiltInNeoPixel = 16;
    const byte BuiltInNeoPixelLength = 1;

    const byte BuiltInA0 = 26;
    const byte BuiltInA1 = 27;
    const byte BuiltInA2 = 28;
    const byte BuiltInA3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 24;
    const byte I2CSCL0 = 25;
    const byte I2CSDA1 = 2;
    const byte I2CSCL1 = 3;

    const byte SPI0Tx = 19;
    const byte SPI0Rx = 20;
    const byte SPI0SCK = 18;
    const byte SPI0SS = 17;

    const byte UART1Tx = 0;
    const byte UART1Rx = 1;

    const byte D0 = 0;   // UART1Tx
    const byte D1 = 1;   // UART1Rx
    const byte D2 = 2;   // I2CSDA1 (STEMMA)
    const byte D3 = 3;   // I2CSCL1 (STEMMA)
    const byte D6 = 6;
    const byte D7 = 7;
    const byte D8 = 8;
    const byte D9 = 9;
    const byte D10 = 10;
    const byte D11 = 11;
    const byte D12 = 12;
    const byte D13 = 13; // BuiltInLED
    const byte D16 = 16; // BuiltInNeoPixel
    const byte D18 = 18; // SPI0SCK
    const byte D19 = 19; // SPI0Tx
    const byte D20 = 20; // SPI0Rx
    const byte D24 = 24; // I2CSDA0
    const byte D25 = 25; // I2CSCL0
    const byte D26 = 26; // BuiltInA0
    const byte D27 = 27; // BuiltInA1
    const byte D28 = 28; // BuiltInA2
    const byte D29 = 29; // BuiltInA3

    uses "/Source/Library/MCU"

    string BoardName { get { return "Adafruit Feather"; } }

}
