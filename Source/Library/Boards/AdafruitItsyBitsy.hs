unit Board // Adafruit ItsyBitsy
{
    #define ADAFRUIT_ITSYBITSY

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP

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

    const byte BuiltInLED = 11;
    const byte BuiltInNeoPixel = 17;
    const byte BuiltInNeoPixelLength = 1;
    const byte BuiltInNeoPixelPower = 16;

    const byte BuiltInA0 = 26;
    const byte BuiltInA1 = 27;
    const byte BuiltInA2 = 28;
    const byte BuiltInA3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 24;
    const byte I2CSCL0 = 25;
    const byte I2CSDA1 = 2;
    const byte I2CSCL1 = 3;

    const byte SPI0Tx = 19;
    const byte SPI0Rx = 20;
    const byte SPI0SCK = 18;
    const byte SPI0SS = 31;

    const byte UART1Tx = 0;
    const byte UART1Rx = 1;


    uses "/Source/Library/MCU"

    string BoardName { get { return "Adafruit ItsyBitsy"; } }

}
