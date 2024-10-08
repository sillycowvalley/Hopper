unit Board // Solderparty RP2350 Stamp Xl
{
    #define SOLDERPARTY_RP2350_STAMP_XL

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2350

    // Note: see Attribution.md (in the same folder as this file)

    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 3;

    const byte A0 = 26;
    const byte A1 = 27;
    const byte A2 = 28;
    const byte A3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 4;
    const byte I2CSCL0 = 5;
    const byte I2CSDA1 = 2;
    const byte I2CSCL1 = 3;

    const byte SPI0Tx = 23;
    const byte SPI0Rx = 20;
    const byte SPI0SCK = 22;
    const byte SPI0SS = 21;
    const byte SPI1Tx = 11;
    const byte SPI1Rx = 8;
    const byte SPI1SCK = 10;
    const byte SPI1SS = 9;

    const byte UART1Tx = 0;
    const byte UART1Rx = 1;
    const byte UART2Tx = 24;
    const byte UART2Rx = 25;


    uses "/Source/Library/MCU"

    string BoardName { get { return "Solderparty RP2350 Stamp Xl"; } }

}
