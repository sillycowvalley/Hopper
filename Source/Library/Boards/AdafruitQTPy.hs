unit Board // Adafruit QT Py
{
    #define ADAFRUIT_QTPY

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040

    #define BOARD_HAS_NEOPIXEL
    #define BOARD_HAS_NEOPIXEL_POWER
    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_NO_SPI1
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 31;
    const byte BuiltInNeoPixel = 12;
    const byte BuiltInNeoPixelLength = 1;
    const byte BuiltInNeoPixelPower = 11;

    const byte BuiltInA0 = 29;
    const byte BuiltInA1 = 28;
    const byte BuiltInA2 = 27;
    const byte BuiltInA3 = 26;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 24;
    const byte I2CSCL0 = 25;
    const byte I2CSDA1 = 22;
    const byte I2CSCL1 = 23;

    const byte SPI0Tx = 3;
    const byte SPI0Rx = 4;
    const byte SPI0SCK = 6;
    const byte SPI0SS = 31;

    const byte UART1Tx = 28;
    const byte UART1Rx = 29;
    const byte UART2Tx = 20;
    const byte UART2Rx = 5;

    const byte D3 = 3;   // SPI0Tx
    const byte D4 = 4;   // SPI0Rx
    const byte D5 = 5;   // UART2Rx
    const byte D6 = 6;   // SPI0SCK
    const byte D11 = 11; // BuiltInNeoPixelPower
    const byte D12 = 12; // BuiltInNeoPixel
    const byte D20 = 20; // UART2Tx
    const byte D21 = 21; // Button
    const byte D22 = 22; // I2CSDA1 (STEMMA)
    const byte D23 = 23; // I2CSCL1 (STEMMA)
    const byte D24 = 24; // I2CSDA0
    const byte D25 = 25; // I2CSCL0
    const byte D26 = 26; // BuiltInA3
    const byte D27 = 27; // BuiltInA2
    const byte D28 = 28; // UART1Tx
    const byte D29 = 29; // UART1Rx

    uses "/Source/Library/MCU"

    string BoardName { get { return "Adafruit QT Py"; } }

}
