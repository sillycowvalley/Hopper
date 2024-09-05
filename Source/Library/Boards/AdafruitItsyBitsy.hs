unit Board // Adafruit ItsyBitsy
{
    #define ADAFRUIT_ITSYBITSY

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040

    // Note: see Attribution.md (in the same folder as this file)

    // https://github.com/adafruit/Adafruit-ItsyBitsy-RP2040-PCB/blob/main/Adafruit%20ItsyBitsy%20RP2040%20pinout.pdf

    #define BOARD_HAS_NEOPIXEL
    #define BOARD_HAS_NEOPIXEL_POWER
    #define BOARD_HAS_LED
    #define BOARD_HAS_USER_BUTTON
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
    const byte UserButton = 13;

    const byte A0 = 26;
    const byte A1 = 27;
    const byte A2 = 28;
    const byte A3 = 29;
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

    const byte GP0 = 0;   // UART1Tx
    const byte GP1 = 1;   // UART1Rx
    const byte GP2 = 2;   // I2CSDA1
    const byte GP3 = 3;   // I2CSCL1
    const byte GP4 = 4;
    const byte GP5 = 5;
    const byte GP6 = 6;
    const byte GP7 = 7;
    const byte GP8 = 8;
    const byte GP9 = 9;
    const byte GP10 = 10;
    const byte GP11 = 11; // BuiltInLED
    const byte GP12 = 12;
    const byte GP13 = 13; // UserButton
    const byte GP14 = 14;
    const byte GP16 = 16; // BuiltInNeoPixelPower
    const byte GP17 = 17; // BuiltInNeoPixel
    const byte GP18 = 18; // SPI0SCK
    const byte GP19 = 19; // SPI0Tx
    const byte GP20 = 20; // SPI0Rx
    const byte GP24 = 24; // I2CSDA0
    const byte GP25 = 25; // I2CSCL0
    const byte GP26 = 26; // A0
    const byte GP27 = 27; // A1
    const byte GP28 = 28; // A2
    const byte GP29 = 29; // A3

    uses "/Source/Library/MCU"

    string BoardName { get { return "Adafruit ItsyBitsy"; } }

}
