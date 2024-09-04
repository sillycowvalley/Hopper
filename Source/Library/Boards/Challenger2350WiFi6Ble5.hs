unit Board // Challenger 2350 WiFi6 Ble5
{
    #define CHALLENGER_2350_WIFI6_BLE5

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP

    #define BOARD_HAS_WIFI
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

    const byte BuiltInLED = 7;

    const byte BuiltInA0 = 29;
    const byte BuiltInA1 = 28;
    const byte BuiltInA2 = 27;
    const byte BuiltInA3 = 26;
    const byte BuiltInA4 = 1;
    const byte BuiltInA5 = 17;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 20;
    const byte I2CSCL0 = 21;

    const byte SPI0Tx = 19;
    const byte SPI0Rx = 16;
    const byte SPI0SCK = 18;
    const byte SPI0SS = 17;
    const byte SPI1Tx = 11;
    const byte SPI1Rx = 8;
    const byte SPI1SCK = 10;
    const byte SPI1SS = 9;

    const byte UART1Tx = 12;
    const byte UART1Rx = 13;
    const byte UART2Tx = 4;
    const byte UART2Rx = 5;

    const byte GP0 = 13;
    const byte GP1 = 12;  // BuiltInA4
    const byte GP5 = 23;  // UART2Rx
    const byte GP6 = 24;
    const byte GP9 = 25;  // SPI1SS
    const byte GP10 = 2;  // SPI1SCK
    const byte GP11 = 3;  // SPI1Tx
    const byte GP12 = 6;  // UART1Tx
    const byte GP13 = 7;  // UART1Rx

    uses "/Source/Library/MCU"

    string BoardName { get { return "Challenger 2350 WiFi6 Ble5"; } }

}
