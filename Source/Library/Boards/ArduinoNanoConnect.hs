unit Board // Arduino Nano Connect
{
    #define ARDUINO_NANO_CONNECT

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP

    // Note: see Attribution.md (in the same folder as this file)

    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 6;

    const byte A0 = 26;
    const byte A1 = 27;
    const byte A2 = 28;
    const byte A3 = 29;
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

    const byte GP0 = 1;   // UART1Tx
    const byte GP1 = 0;   // UART1Rx
    const byte GP2 = 25;
    const byte GP3 = 15;
    const byte GP4 = 16;  // SPI0Rx
    const byte GP5 = 17;  // SPI1SS
    const byte GP6 = 18;  // SPI0SCK
    const byte GP7 = 19;  // SPI0Tx
    const byte GP8 = 20;  // UART2Tx
    const byte GP9 = 21;  // UART2Rx
    const byte GP10 = 5;
    const byte GP11 = 7;  // SPI1Tx
    const byte GP12 = 4;  // I2CSDA0
    const byte GP13 = 6;  // I2CSCL0
    const byte GP14 = 26; // SPI1SCK
    const byte GP15 = 27;
    const byte GP16 = 28;
    const byte GP17 = 29;
    const byte GP18 = 12;
    const byte GP19 = 13;
    const byte GP20 = 2;
    const byte GP21 = 24;
    const byte GP22 = 22;
    const byte GP23 = 23;
    const byte GP24 = 3;
    const byte GP25 = 8;
    const byte GP26 = 9;  // I2CSDA1
    const byte GP27 = 10; // I2CSCL1
    const byte GP28 = 11; // A2
    const byte GP29 = 14; // A3

    uses "/Source/Library/MCU"

    string BoardName { get { return "Arduino Nano Connect"; } }

}
