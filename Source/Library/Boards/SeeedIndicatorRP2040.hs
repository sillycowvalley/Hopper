unit Board // Seeed Indicator RP2040
{
    #define SEEED_INDICATOR_RP2040

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040

    #define BOARD_HAS_I2C
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInA0 = 26;
    const byte BuiltInA1 = 27;
    const byte BuiltInA2 = 28;
    const byte BuiltInA3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 20;
    const byte I2CSCL0 = 21;
    const byte I2CSDA1 = 14;
    const byte I2CSCL1 = 15;

    const byte SPI0Tx = 3;
    const byte SPI0Rx = 0;
    const byte SPI0SCK = 2;
    const byte SPI0SS = 1;
    const byte SPI1Tx = 11;
    const byte SPI1Rx = 12;
    const byte SPI1SCK = 10;
    const byte SPI1SS = 13;

    const byte UART1Tx = 16;
    const byte UART1Rx = 17;
    const byte UART2Tx = 8;
    const byte UART2Rx = 9;


    uses "/Source/Library/MCU"
}
