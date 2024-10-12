unit Board // Adafruit Feather RP2350 Hstx
{
    #define ADAFRUIT_FEATHER_RP2350_HSTX

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2350

    // Note: see Attribution.md (in the same folder as this file)
    // https://learn.adafruit.com/adafruit-feather-rp2040-pico/pinouts

    #define FORMFACTOR_FEATHER

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

    const byte BuiltInLED = 7;
    const byte BuiltInNeoPixel = 21;
    const byte BuiltInNeoPixelLength = 1;

    const byte A0 = 26;
    const byte A1 = 27;
    const byte A2 = 28;
    const byte A3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 2;
    const byte I2CSCL0 = 3;

    const byte SPI0Tx = 23;
    const byte SPI0Rx = 20;
    const byte SPI0SCK = 22;
    const byte SPI0SS = 21;

    const byte UART1Tx = 0;
    const byte UART1Rx = 1;


    uses "/Source/Library/MCU"

    string BoardName { get { return "Adafruit Feather RP2350 Hstx"; } }

}
