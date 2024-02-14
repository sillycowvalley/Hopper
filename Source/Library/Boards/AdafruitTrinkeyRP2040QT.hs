unit Board // Adafruit TrinkeyRP2040QT
{
    #define ADAFRUIT_TRINKEYRP2040QT

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040
    #define FORMFACTOR_USBA_STICK

    #define BOARD_HAS_NEOPIXEL
    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_NO_I2C1
    #define BOARD_HAS_NO_SPI0
    #define BOARD_HAS_NO_SPI1
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 31;
    const byte BuiltInNeoPixel = 27;
    const byte BuiltInNeoPixelLength = 1;

    const byte BuiltInA0 = 26;
    const byte BuiltInA1 = 27;
    const byte BuiltInA2 = 28;
    const byte BuiltInA3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 16;
    const byte I2CSCL0 = 17;

    const byte UART1Tx = 16;
    const byte UART1Rx = 17;

    const byte D12 = 12; // Button
    const byte D16 = 16; // UART1Tx (STEMMA)
    const byte D17 = 17; // UART1Rx (STEMMA)
    const byte D27 = 27;

    uses "/Source/Library/MCU"
}
