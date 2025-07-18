unit Board // PiPico
{
    #define RPI

    #define SBC_BOARD_DEFINED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_SPI
    #define BOARD_HAS_HARDWARE_SPI // no CS
    #define BOARD_HAS_LED
    
    #define FORMFACTOR_PI
    
    const byte BuiltInLED = 4;
    
    const byte I2CSDA0 = 2;
    const byte I2CSCL0 = 3;
    
    const byte SPI0Tx = 10;
    const byte SPI0Rx = 9;
    const byte SPI0SCK = 11;
    const byte SPI0SS = 8;
    const byte SPI1Tx = 20;
    const byte SPI1Rx = 19;
    const byte SPI1SCK = 21;
    const byte SPI1SS = 7;
    
    //const byte GP0 = 0;
    //const byte GP1 = 1;
    const byte GP2 = 2;
    const byte GP3 = 3;
    const byte GP4 = 4;
    const byte GP5 = 5;
    const byte GP6 = 6;
    const byte GP7 = 7;
    const byte GP8 = 8;
    const byte GP9 = 9;
    const byte GP10 = 10;
    const byte GP11 = 11;
    const byte GP12 = 12;
    const byte GP13 = 13;
    const byte GP14 = 14;
    const byte GP15 = 15;
    const byte GP16 = 16;
    const byte GP17 = 17;
    const byte GP18 = 18;
    const byte GP19 = 19;
    const byte GP20 = 20;
    const byte GP21 = 21;
    const byte GP22 = 22;
    const byte GP23 = 23;
    const byte GP24 = 24;
    const byte GP25 = 25;
    const byte GP26 = 26;
    const byte GP27 = 27;

    uses "/Source/Library/SBC"

    string BoardName { get { return "Pi"; } }

}
