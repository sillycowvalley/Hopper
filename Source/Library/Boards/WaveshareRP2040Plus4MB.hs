unit Board // Waveshare RP2040 Plus 4MB
{
    #define WAVESHARE_RP2040_PLUS_4MB

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040

    // Note: see Attribution.md (in the same folder as this file)
    // https://mischianti.org/raspberry-pi-pico-high-resolution-pinout-and-specs/

    #define FORMFACTOR_PICO

    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 25;

    const byte A0 = 26;
    const byte A1 = 27;
    const byte A2 = 28;
    const byte A3 = 29;
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

    const byte GP0 = 0;   // UART1Tx
    const byte GP1 = 1;   // UART1Rx
    const byte GP2 = 2;
    const byte GP3 = 3;
    const byte GP4 = 4;
    const byte GP5 = 5;
    const byte GP6 = 6;   // I2CSDA1
    const byte GP7 = 7;   // I2CSCL1
    const byte GP8 = 8;   // UART2Tx
    const byte GP9 = 9;   // UART2Rx
    const byte GP10 = 10;
    const byte GP11 = 11;
    const byte GP12 = 12; // SPI1Rx
    const byte GP13 = 13; // SPI1SS
    const byte GP14 = 14; // SPI1SCK
    const byte GP15 = 15; // SPI1Tx
    const byte GP16 = 16; // SPI0Rx
    const byte GP17 = 17; // SPI0SS
    const byte GP18 = 18; // SPI0SCK
    const byte GP19 = 19; // SPI0Tx
    const byte GP20 = 20;
    const byte GP21 = 21;
    const byte GP22 = 22;
    const byte GP23 = 23; // output: controls the on-board SMPS power save pin
    const byte GP24 = 24; // input:  VBUS sense: true if VBUS present, otherwise false
    const byte GP25 = 25; // BuiltInLED
    const byte GP26 = 26; // A0
    const byte GP27 = 27; // A1
    const byte GP28 = 28; // A2
    const byte GP29 = 29; // A3 (input:  used in ADC mode (A3) to measure VSYS/3)

    uses "/Source/Library/MCU"

    string BoardName { get { return "Waveshare RP2040 Plus 4MB"; } }

}
