unit Board // Challenger 2040 WiFi
{
    #define CHALLENGER_2040_WIFI

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040

    #define BOARD_HAS_WIFI
    #define BOARD_HAS_NEOPIXEL
    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_NO_I2C1
    #define BOARD_HAS_NO_SPI1
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3
    #define BOARD_HAS_A4
    #define BOARD_HAS_A5

    const byte BuiltInLED = 12;
    const byte BuiltInNeoPixel = 11;
    const byte BuiltInNeoPixelLength = 1;

    const byte BuiltInA0 = 26;
    const byte BuiltInA1 = 27;
    const byte BuiltInA2 = 28;
    const byte BuiltInA3 = 29;
    const byte BuiltInA4 = 25;
    const byte BuiltInA5 = 21;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 0;
    const byte I2CSCL0 = 1;

    const byte SPI0Tx = 23;
    const byte SPI0Rx = 24;
    const byte SPI0SCK = 22;
    const byte SPI0SS = 21;

    const byte UART1Tx = 16;
    const byte UART1Rx = 17;
    const byte UART2Tx = 4;
    const byte UART2Rx = 5;

    const byte D0 = 16;  // I2CSDA0
    const byte D1 = 17;  // I2CSCL0
    const byte D2 = 24;
    const byte D3 = 23;
    const byte D4 = 22;  // UART2Tx
    const byte D5 = 2;   // UART2Rx
    const byte D6 = 3;
    const byte D7 = 0;
    const byte D8 = 1;
    const byte D9 = 6;
    const byte D10 = 7;
    const byte D11 = 8;  // BuiltInNeoPixel
    const byte D12 = 9;  // BuiltInLED
    const byte D13 = 10;
    const byte D14 = 11;
    const byte D15 = 12;
    const byte D16 = 13; // UART1Tx
    const byte D17 = 19; // UART1Rx

    uses "/Source/Library/MCU"

    string BoardName { get { return "Challenger 2040 WiFi"; } }

}
