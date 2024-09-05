unit Board // DFRobot Beetle RP2040
{
    #define DFROBOT_BEETLE_RP2040

    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040

    // Note: see Attribution.md (in the same folder as this file)

    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_NO_SPI1
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3

    const byte BuiltInLED = 13;

    const byte A0 = 26;
    const byte A1 = 27;
    const byte A2 = 28;
    const byte A3 = 29;
    const byte ADCResolution = 12;

    const byte I2CSDA0 = 4;
    const byte I2CSCL0 = 5;
    const byte I2CSDA1 = 2;
    const byte I2CSCL1 = 3;

    const byte SPI0Tx = 3;
    const byte SPI0Rx = 0;
    const byte SPI0SCK = 2;
    const byte SPI0SS = 1;

    const byte UART1Tx = 28;
    const byte UART1Rx = 29;
    const byte UART2Tx = 4;
    const byte UART2Rx = 5;


    uses "/Source/Library/MCU"

    string BoardName { get { return "DFRobot Beetle RP2040"; } }

}
