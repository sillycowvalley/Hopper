unit Board // Challenger 2040 LoRa
{
    #define CHALLENGER_2040_LORA

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP

    // Note: see Attribution.md (in the same folder as this file)

    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_NO_I2C1
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3
    #define BOARD_HAS_A4
    #define BOARD_HAS_A5

    const byte BuiltInLED = 24;

    const byte A0 = 26;
    const byte A1 = 27;
    const byte A2 = 28;
    const byte A3 = 29;
    const byte A4 = 19;
    const byte A5 = 21;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 0;
    const byte I2CSCL0 = 1;

    const byte SPI0Tx = 23;
    const byte SPI0Rx = 20;
    const byte SPI0SCK = 22;
    const byte SPI0SS = 21;
    const byte SPI1Tx = 11;
    const byte SPI1Rx = 12;
    const byte SPI1SCK = 10;
    const byte SPI1SS = 9;

    const byte UART1Tx = 16;
    const byte UART1Rx = 17;

    const byte GP0 = 16;  // I2CSDA0
    const byte GP1 = 17;  // I2CSCL0
    const byte GP2 = 20;
    const byte GP3 = 23;
    const byte GP4 = 22;
    const byte GP5 = 2;
    const byte GP6 = 3;
    const byte GP7 = 0;
    const byte GP8 = 1;
    const byte GP9 = 4;   // SPI1SS
    const byte GP10 = 5;  // SPI1SCK
    const byte GP11 = 6;  // SPI1Tx
    const byte GP12 = 7;  // SPI1Rx
    const byte GP13 = 8;
    const byte GP14 = 13;
    const byte GP15 = 14;
    const byte GP16 = 15; // UART1Tx
    const byte GP17 = 18; // UART1Rx
    const byte GP18 = 24;

    uses "/Source/Library/MCU"

    string BoardName { get { return "Challenger 2040 LoRa"; } }

}
