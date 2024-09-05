unit Board // Adafruit QT Py
{
    #define ADAFRUIT_QTPY

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040

    // Note: see Attribution.md (in the same folder as this file)

    // https://github.com/adafruit/Adafruit-QT-Py-RP2040-PCB/blob/main/Adafruit%20QT%20Py%20RP2040%20Pinout.pdf

    #define BOARD_HAS_NEOPIXEL
    #define BOARD_HAS_NEOPIXEL_POWER
    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_NO_SPI1
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 31;
    const byte BuiltInNeoPixel = 12;
    const byte BuiltInNeoPixelLength = 1;
    const byte BuiltInNeoPixelPower = 11;

    const byte A0 = 29;
    const byte A1 = 28;
    const byte A2 = 27;
    const byte A3 = 26;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 24;
    const byte I2CSCL0 = 25;
    const byte I2CSDA1 = 22;
    const byte I2CSCL1 = 23;

    const byte SPI0Tx = 3;
    const byte SPI0Rx = 4;
    const byte SPI0SCK = 6;
    const byte SPI0SS = 31;

    const byte UART1Tx = 28;
    const byte UART1Rx = 29;
    const byte UART2Tx = 20;
    const byte UART2Rx = 5;

    const byte GP3 = 3;   // SPI0Tx
    const byte GP4 = 4;   // SPI0Rx
    const byte GP5 = 5;   // UART2Rx
    const byte GP6 = 6;   // SPI0SCK
    const byte GP11 = 11; // BuiltInNeoPixelPower
    const byte GP12 = 12; // BuiltInNeoPixel
    const byte GP20 = 20; // UART2Tx
    const byte GP21 = 21; // Button
    const byte GP22 = 22; // I2CSDA1 (STEMMA)
    const byte GP23 = 23; // I2CSCL1 (STEMMA)
    const byte GP24 = 24; // I2CSDA0
    const byte GP25 = 25; // I2CSCL0
    const byte GP26 = 26; // A3
    const byte GP27 = 27; // A2
    const byte GP28 = 28; // UART1Tx
    const byte GP29 = 29; // UART1Rx

    uses "/Source/Library/MCU"

    string BoardName { get { return "Adafruit QT Py"; } }

}
