unit Board // Waveshare RP2040 Plus 16MB
{
    #define WAVESHARE_RP2040_PLUS_16MB

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040
    #define FORMFACTOR_PICO

    // https://mischianti.org/raspberry-pi-pico-high-resolution-pinout-and-specs/

    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 25;

    const byte BuiltInA0 = 26;
    const byte BuiltInA1 = 27;
    const byte BuiltInA2 = 28;
    const byte BuiltInA3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 8;
    const byte I2CSCL0 = 9;
    const byte I2CSDA1 = 6;
    const byte I2CSCL1 = 7;

    const byte SPI0Tx = 19;
    const byte SPI0Rx = 16;
    const byte SPI0SCK = 18;
    const byte SPI0SS = 17;
    const byte SPI1Tx = 15;
    const byte SPI1Rx = 12;
    const byte SPI1SCK = 14;
    const byte SPI1SS = 13;

    const byte UART1Tx = 0;
    const byte UART1Rx = 1;
    const byte UART2Tx = 8;
    const byte UART2Rx = 9;

    const byte D0 = 0;   // UART1Tx
    const byte D1 = 1;   // UART1Rx
    const byte D2 = 2;
    const byte D3 = 3;
    const byte D4 = 4;
    const byte D5 = 5;
    const byte D6 = 6;   // I2CSDA1
    const byte D7 = 7;   // I2CSCL1
    const byte D8 = 8;   // UART2Tx
    const byte D9 = 9;   // UART2Rx
    const byte D10 = 10;
    const byte D11 = 11;
    const byte D12 = 12; // SPI1Rx
    const byte D13 = 13; // SPI1SS
    const byte D14 = 14; // SPI1SCK
    const byte D15 = 15; // SPI1Tx
    const byte D16 = 16; // SPI0Rx
    const byte D17 = 17; // SPI0SS
    const byte D18 = 18; // SPI0SCK
    const byte D19 = 19; // SPI0Tx
    const byte D20 = 20;
    const byte D21 = 21;
    const byte D22 = 22;
    const byte D23 = 23; // output: controls the on-board SMPS power save pin
    const byte D24 = 24; // input:  VBUS sense: true if VBUS present, otherwise false
    const byte D25 = 25; // BuiltInLED
    const byte D26 = 26; // BuiltInA0
    const byte D27 = 27; // BuiltInA1
    const byte D28 = 28; // BuiltInA2
    const byte D29 = 29; // BuiltInA3 (input:  used in ADC mode (A3) to measure VSYS/3)

    uses "/Source/Library/MCU"

    string BoardName { get { return "Waveshare RP2040 Plus 16MB"; } }

}
