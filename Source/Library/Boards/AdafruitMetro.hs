unit Board // Adafruit Metro
{
    #define ADAFRUIT_METRO

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040

    #define BOARD_HAS_NEOPIXEL
    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_NO_SPI1
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 13;
    const byte BuiltInNeoPixel = 14;
    const byte BuiltInNeoPixelLength = 1;

    const byte BuiltInA0 = 26;
    const byte BuiltInA1 = 27;
    const byte BuiltInA2 = 28;
    const byte BuiltInA3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 16;
    const byte I2CSCL0 = 17;
    const byte I2CSDA1 = 2;
    const byte I2CSCL1 = 3;

    const byte SPI0Tx = 19;
    const byte SPI0Rx = 20;
    const byte SPI0SCK = 18;
    const byte SPI0SS = 23;

    const byte UART1Tx = 0;
    const byte UART1Rx = 1;


    uses "/Source/Library/MCU"

    string BoardName { get { return "Adafruit Metro"; } }

}
