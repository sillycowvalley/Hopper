unit Board // Arduino Nano Connect
{
    #define ARDUINO_NANO_CONNECT

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040

    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 6;

    const byte BuiltInA0 = 26;
    const byte BuiltInA1 = 27;
    const byte BuiltInA2 = 28;
    const byte BuiltInA3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 12;
    const byte I2CSCL0 = 13;
    const byte I2CSDA1 = 26;
    const byte I2CSCL1 = 27;

    const byte SPI0Tx = 7;
    const byte SPI0Rx = 4;
    const byte SPI0SCK = 6;
    const byte SPI0SS = 5;
    const byte SPI1Tx = 11;
    const byte SPI1Rx = 8;
    const byte SPI1SCK = 14;
    const byte SPI1SS = 5;

    const byte UART1Tx = 0;
    const byte UART1Rx = 1;
    const byte UART2Tx = 8;
    const byte UART2Rx = 9;

    const byte D0 = 1;   // UART1Tx
    const byte D1 = 0;   // UART1Rx
    const byte D2 = 25;
    const byte D3 = 15;
    const byte D4 = 16;  // SPI0Rx
    const byte D5 = 17;  // SPI1SS
    const byte D6 = 18;  // SPI0SCK
    const byte D7 = 19;  // SPI0Tx
    const byte D8 = 20;  // UART2Tx
    const byte D9 = 21;  // UART2Rx
    const byte D10 = 5;
    const byte D11 = 7;  // SPI1Tx
    const byte D12 = 4;  // I2CSDA0
    const byte D13 = 6;  // I2CSCL0
    const byte D14 = 26; // SPI1SCK
    const byte D15 = 27;
    const byte D16 = 28;
    const byte D17 = 29;
    const byte D18 = 12;
    const byte D19 = 13;
    const byte D20 = 2;
    const byte D21 = 24;
    const byte D22 = 22;
    const byte D23 = 23;
    const byte D24 = 3;
    const byte D25 = 8;
    const byte D26 = 9;  // I2CSDA1
    const byte D27 = 10; // I2CSCL1
    const byte D28 = 11; // BuiltInA2
    const byte D29 = 14; // BuiltInA3

    uses "/Source/Library/MCU"
}
