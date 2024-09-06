unit Board // Challenger NB 2040 WiFi
{
    #define CHALLENGER_NB_2040_WIFI

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040

    // Note: see Attribution.md (in the same folder as this file)

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

    const byte A0 = 26;
    const byte A1 = 27;
    const byte A2 = 28;
    const byte A3 = 29;
    const byte A4 = 25;
    const byte A5 = 21;
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

    const byte GP26 = 26; // A0
    const byte GP27 = 27; // A1
    const byte GP28 = 28; // A2
    const byte GP29 = 29; // A3
    const byte GP25 = 25; // A4
    const byte GP21 = 21; // SPI0SS
    const byte GP22 = 22; // SPI0SCK
    const byte GP23 = 23; // SPI0Tx
    const byte GP20 = 20;
    const byte GP17 = 17; // UART1Rx
    const byte GP16 = 16; // UART1Tx
    const byte GP10 = 10;
    const byte GP9 = 9;
    const byte GP8 = 8;
    const byte GP7 = 7;
    const byte GP6 = 6;
    const byte GP3 = 3;
    const byte GP2 = 2;
    const byte GP1 = 1;   // I2CSCL0
    const byte GP0 = 0;   // I2CSDA0
    const byte GP18 = 18;
    const byte GP15 = 15;
    const byte GP14 = 14;

    uses "/Source/Library/MCU"

    string BoardName { get { return "Challenger NB 2040 WiFi"; } }

}
