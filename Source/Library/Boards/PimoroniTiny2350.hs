unit Board // Pimoroni Tiny2350
{
    #define PIMORONI_TINY2350

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2350

    // Note: see Attribution.md (in the same folder as this file)

    #define BOARD_HAS_LED
    #define BOARD_HAS_RGBLED
    #define BOARD_HAS_USER_BUTTON
    #define BOARD_HAS_I2C
    #define BOARD_HAS_NO_SPI1
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 19;
    const byte BuiltInLEDR = 18;
    const byte BuiltInLEDG = 19;
    const byte BuiltInLEDB = 20;
    const byte UserButton = 23;

    const byte A0 = 26;
    const byte A1 = 27;
    const byte A2 = 28;
    const byte A3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 4;
    const byte I2CSCL0 = 5;
    const byte I2CSDA1 = 6;
    const byte I2CSCL1 = 7;

    const byte SPI0Tx = 7;
    const byte SPI0Rx = 4;
    const byte SPI0SCK = 6;
    const byte SPI0SS = 17;

    const byte UART1Tx = 0;
    const byte UART1Rx = 1;
    const byte UART2Tx = 4;
    const byte UART2Rx = 5;

    const byte GP0 = 0;   // UART1Tx
    const byte GP1 = 1;   // UART1Rx
    const byte GP2 = 2;
    const byte GP3 = 3;
    const byte GP4 = 4;   // UART2Tx
    const byte GP5 = 5;   // UART2Rx
    const byte GP6 = 6;   // SPI0SCK
    const byte GP7 = 7;   // SPI0Tx
    const byte GP18 = 18; // BuiltInLEDR
    const byte GP19 = 19; // BuiltInLEDG
    const byte GP20 = 20; // BuiltInLEDB
    const byte GP23 = 23; // UserButton
    const byte GP26 = 26; // A0
    const byte GP27 = 27; // A1
    const byte GP28 = 28; // A2
    const byte GP29 = 29; // A3

    uses "/Source/Library/MCU"

    string BoardName { get { return "Pimoroni Tiny2350"; } }

}
